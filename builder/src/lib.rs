use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, DeriveInput, Ident, MetaNameValue, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_builder(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    expand_derive(ast).unwrap_or_else(|err| err.to_compile_error().into())
}

fn expand_derive(input: DeriveInput) -> Result<TokenStream, syn::Error> {
    let ast = input;
    let name = &ast.ident;
    let builder_name = format!("{}{}", name, "Builder");

    let struct_ident = Ident::new(&builder_name, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        return Err(syn::Error::new_spanned(
            ast,
            "Builder can only be derived for structs with named fields",
        ));
    };

    let is_type = |field: &syn::Field, type_id: &str| {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
            if let Some(segment) = path.segments.last() {
                if segment.ident == type_id {
                    return Ok(true);
                }
            }
        } else {
            return Err(syn::Error::new_spanned(&field.ty, "Expected a type path"));
        }
        return Ok(false);
    };

    let mut options: Vec<syn::Field> = vec![];
    for f in fields.iter() {
        let ty = &f.ty;

        let mut wrapped: syn::Type = parse_quote!(std::option::Option<#ty>);
        if is_type(&f, "Option")? {
            wrapped = parse_quote!(#ty);
        }
        let filtered_attrs: Vec<_> = f
            .attrs
            .iter()
            .filter(|attr| !attr.path().is_ident("builder"))
            .cloned()
            .collect();

        let opion_field = syn::Field {
            attrs: filtered_attrs,
            vis: f.vis.clone(),
            mutability: f.mutability.clone(),
            ident: f.ident.clone(),
            colon_token: f.colon_token.clone(),
            ty: wrapped,
        };
        options.push(opion_field);
    }

    let mut nones: Vec<syn::Field> = vec![];
    for f in fields.iter() {
        let filtered_attrs: Vec<_> = f
            .attrs
            .iter()
            .filter(|attr| !attr.path().is_ident("builder"))
            .cloned()
            .collect();
        let none = syn::Field {
            attrs: filtered_attrs,
            vis: f.vis.clone(),
            mutability: f.mutability.clone(),
            ident: f.ident.clone(),
            colon_token: f.colon_token.clone(),
            ty: parse_quote!(std::option::Option::None),
        };
        nones.push(none);
    }

    let mut setters: Vec<_> = vec![];
    for f in fields.iter() {
        let ident = &f.ident;
        let mut ty = &f.ty;
        if is_type(&f, "Option")? {
            let inner = extract_option_inner(&ty).expect("Failed to extract inner type");
            ty = inner;
        }
        if f.attrs.iter().any(|attr| attr.path().is_ident("builder")) {
            let attr = f
                .attrs
                .iter()
                .find(|attr| attr.path().is_ident("builder"))
                .expect("Failed to find builder attribute");

            let meta: std::result::Result<MetaNameValue, syn::Error> = attr.parse_args();
            if meta.is_err() {
                return Err(syn::Error::new_spanned(
                    &attr.meta,
                    "attribute content needs to be set, like `builder(each = \"...\")`",
                ));
            }
            let meta = meta.unwrap();
            let syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(ref lit_str),
                ..
            }) = meta.value
            else {
                return Err(syn::Error::new_spanned(
                    &meta.value,
                    "expected string literal as value",
                ));
            };
            if lit_str.value() == ident.as_ref().unwrap().to_string() {
                continue;
            }
        }
        let func = quote! {
                pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                self
            }
        };
        setters.push(func);
    }

    let arg_setters = fields
        .iter()
        .filter_map(|f| vec_single_setter(&f.clone()))
        .collect::<Vec<_>>();

    let mut builders: Vec<_> = vec![];
    for f in fields.iter() {
        let ident = &f.ident;
        if is_type(&f, "Option")? {
            builders.push(quote! {
            #ident: self.#ident.clone()});
        } else if is_type(&f, "Vec")? {
            builders.push(quote! {
            #ident: self.#ident.clone().unwrap_or_else(Vec::new)});
        } else {
            builders.push(quote! {
                #ident: self.#ident.clone().ok_or(format!("Field {} is not set", stringify!(#ident)))?});
        }
    }

    let stream = quote! {
        pub struct #struct_ident {
            #(#options,)*
        }
        impl #name {
            pub fn builder() -> #struct_ident {
                #struct_ident {
                    #(#nones,)*
                }
            }
        }
        impl #struct_ident {
            #(#setters)*
            #(#arg_setters)*


        pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
            Ok(#name {
                #(#builders,)*
            })
        }
        }
    };
    Ok(stream.into())
}

fn extract_option_inner(ty: &Type) -> std::option::Option<&Type> {
    let Type::Path(type_path) = ty else {
        return std::option::Option::None;
    };

    if type_path.qself.is_some() {
        return std::option::Option::None;
    }

    let segment = type_path.path.segments.last()?;

    if segment.ident != "Option" {
        return std::option::Option::None;
    }

    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return std::option::Option::None;
    };

    let std::option::Option::Some(syn::GenericArgument::Type(inner)) = args.args.last() else {
        return std::option::Option::None;
    };

    std::option::Option::Some(inner)
}

fn vec_single_setter(field: &syn::Field) -> std::option::Option<proc_macro2::TokenStream> {
    for attr in &field.attrs {
        if !attr.path().is_ident("builder") {
            continue;
        }
        let meta: MetaNameValue = attr.parse_args().ok()?;
        if !meta.path.is_ident("each") {
            let err = syn::Error::new_spanned(&attr.meta, "expected `builder(each = \"...\")`");
            return std::option::Option::Some(err.to_compile_error());
        }
        let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(lit_str),
            ..
        }) = meta.value
        else {
            continue;
        };
        let method_name = Ident::new(&lit_str.value(), lit_str.span());
        let field_ident = &field.ident;
        let inner = extract_inner(&field.ty, "Vec")?;
        return std::option::Option::Some(quote! {
            pub fn #method_name(&mut self, #method_name: #inner) -> &mut Self {
                if self.#field_ident.is_none() {
                    self.#field_ident = std::option::Option::Some(Vec::new());
                }
                if let std::option::Option::Some(ref mut vec) = self.#field_ident {
                    vec.push(#method_name);
                }
                self
            }
        });
    }

    std::option::Option::None
}

fn extract_inner(ty: &syn::Type, type_str: &str) -> std::option::Option<syn::Type> {
    let Type::Path(type_path) = ty else {
        return std::option::Option::None;
    };

    let segment = type_path.path.segments.last()?;

    if segment.ident != type_str {
        return std::option::Option::None;
    }

    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return std::option::Option::None;
    };

    let std::option::Option::Some(syn::GenericArgument::Type(inner)) = args.args.last() else {
        return std::option::Option::None;
    };

    std::option::Option::Some(inner.clone())
}

fn inner_type(ty: syn::Type) -> std::option::Option<syn::Type> {
    let Type::Path(type_path) = ty else {
        return std::option::Option::None;
    };

    if type_path.qself.is_some() {
        return std::option::Option::None;
    }
    let segment = type_path.path.segments.last()?;

    if segment.ident != "std::option::Option" {
        return std::option::Option::None;
    }

    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return std::option::Option::None;
    };

    let std::option::Option::Some(syn::GenericArgument::Type(inner)) = args.args.last() else {
        return std::option::Option::None;
    };

    std::option::Option::Some(inner.clone())
}
