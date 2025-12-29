use proc_macro::TokenStream;
use quote::quote;
use syn::visit::{self, Visit};
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, DeriveInput, ExprLit, GenericParam, Generics,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    return expand_macro(ast).unwrap_or_else(|e| e.to_compile_error().into());
}

fn custom_debug_attr(field: &syn::Field) -> Option<proc_macro2::TokenStream> {
    for attr in &field.attrs {
        if attr.path().is_ident("debug") {
            let meta = attr.meta.clone();
            if let syn::Meta::NameValue(meta) = meta {
                let val = meta.value;
                if let syn::Expr::Lit(ExprLit {
                    lit: syn::Lit::Str(lit_str),
                    ..
                }) = val
                {
                    let ident = field.ident.as_ref().unwrap();
                    let value: proc_macro2::TokenStream = parse_quote! { &self.#ident };
                    let fmt_str = lit_str.value();
                    return Some(quote! {
                        &format_args!(#fmt_str, #value)
                    });
                }
            }
        }
    }
    None
}

fn add_trait_bounds(mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            type_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

fn generic_used_in_fields(data: &syn::Data, generics: &Generics) -> bool {
    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = data {
        match fields {
            syn::Fields::Named(fields_named) => {
                for param in generics.params.iter() {
                    if let GenericParam::Type(type_param) = param {
                        for p in fields_named.named.iter() {
                            if let true = is_type_inner(p, &type_param.ident.to_string()) {
                                println!(
                                    "Generic {} used in field {:?}",
                                    type_param.ident, p.ident
                                );
                                return true;
                            }
                        }
                    }
                }
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                for param in generics.params.iter() {
                    if let GenericParam::Type(type_param) = param {
                        for p in fields_unnamed.unnamed.iter() {
                            if let true = is_type_inner(p, &type_param.ident.to_string()) {
                                return true;
                            }
                        }
                    }
                }
            }
            syn::Fields::Unit => {}
        }
    }
    false
}

struct TypeFinder<'a> {
    target: &'a str,
    found: bool,
}

impl<'ast> Visit<'ast> for TypeFinder<'_> {
    fn visit_path(&mut self, path: &'ast syn::Path) {
        if let Some(first) = path.segments.first() {
            if first.ident == "PhantomData" {
                return;
            }

            if first.ident == self.target && path.segments.len() == 1 {
                self.found = true;
                return;
            }
        }

        visit::visit_path(self, path);
    }
}

fn is_type_inner(field: &syn::Field, type_id: &str) -> bool {
    let mut finder = TypeFinder {
        target: type_id,
        found: false,
    };
    finder.visit_type(&field.ty);
    finder.found
}

struct AssociatedTypeFinder<'a> {
    target: &'a str,
    found_paths: Vec<syn::Path>,
}

impl<'ast> Visit<'ast> for AssociatedTypeFinder<'_> {
    fn visit_path(&mut self, path: &'ast syn::Path) {
        if let Some(segment) = path.segments.first() {
            if segment.ident == self.target && path.segments.len() > 1 {
                self.found_paths.push(path.clone());
                return;
            }
        }

        visit::visit_path(self, path);
    }
}

fn get_associated_types(data: &syn::Data, type_param_name: &str) -> Vec<syn::Path> {
    let mut finder = AssociatedTypeFinder {
        target: type_param_name,
        found_paths: vec![],
    };

    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = data {
        match fields {
            syn::Fields::Named(fields_named) => {
                for field in &fields_named.named {
                    finder.visit_type(&field.ty);
                }
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                for field in &fields_unnamed.unnamed {
                    finder.visit_type(&field.ty);
                }
            }
            syn::Fields::Unit => {}
        }
    }

    finder.found_paths
}

fn expand_macro(input: DeriveInput) -> syn::Result<TokenStream> {
    let name = &input.ident;
    let data = &input.data;
    let struct_name = syn::LitStr::new(&name.to_string(), name.span());
    let mut generics: Generics;
    if generic_used_in_fields(data, &input.generics) {
        generics = add_trait_bounds(input.generics.clone());
    } else {
        generics = input.generics;
    }

    let mut all_associated_types = Vec::new();
    for param in generics.params.iter() {
        if let GenericParam::Type(type_param) = param {
            let associated_types = get_associated_types(data, &type_param.ident.to_string());
            all_associated_types.extend(associated_types);
        }
    }

    if !all_associated_types.is_empty() {
        let where_clause = generics.make_where_clause();
        for assoc_type in all_associated_types {
            where_clause
                .predicates
                .push(parse_quote! { #assoc_type: std::fmt::Debug });
        }
    }

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    if let syn::Data::Struct(syn::DataStruct { fields, .. }) = data {
        match fields {
            syn::Fields::Named(fields_named) => {
                let mut fields = vec![];
                for p in fields_named.named.clone() {
                    let ident = p.ident.clone().unwrap();
                    let field = syn::LitStr::new(&ident.to_string(), ident.span());
                    let custom_debug = custom_debug_attr(&p);
                    if let Some(custom_debug) = custom_debug {
                        fields.push(quote! { .field(#field, #custom_debug) }.into());
                    } else {
                        fields.push(quote! { .field(#field, &self.#ident)
                        })
                    }
                }
                return Ok(quote! {
                    impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            f.debug_struct(#struct_name)
                            #(#fields)*
                            .finish()
                        }
                    }
                }
                .into());
            }
            syn::Fields::Unnamed(fields_unnamed) => {
                let mut fields = vec![];
                for (i, p) in fields_unnamed.unnamed.iter().enumerate() {
                    let field = syn::LitStr::new(&i.to_string(), p.span());
                    let suffix = syn::Index::from(i);
                    fields.push(quote! { .field(#field, &self.#suffix) });
                }
                return Ok(quote! {
                    impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
                        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                            f.debug_struct(#struct_name)
                            #(#fields)*
                            .finish()
                        }
                    }
                }
                .into());
            }
            syn::Fields::Unit => {
                let name_str = syn::LitStr::new(&name.to_string(), name.span());
                return Ok(quote! {

                impl std::fmt::Debug for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        f.debug_struct(#name_str).finish()
                    }
                }
                }
                .into());
            }
        }
    }

    unreachable!()
}
