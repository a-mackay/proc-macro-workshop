extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Field};

fn is_option_type(ty: &syn::Type) -> bool {
    get_type_within_option(ty).is_some()
}

fn get_type_within(ty: &syn::Type, outer_type_name: &str) -> Option<syn::Type> {
    use syn::{Type, TypePath, Path, PathSegment, PathArguments, AngleBracketedGenericArguments, GenericArgument};

    match ty {
        Type::Path(TypePath {
            qself: None,
            path: Path {
                segments, ..
            },
        }) => {
            if segments.len() == 1 {
                let PathSegment { ident, arguments, .. } = segments.first().unwrap();
                if ident.to_string() == outer_type_name {
                    match arguments {
                        PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) => {
                            if args.len() == 1 {
                                match args.first().unwrap() {
                                    GenericArgument::Type(inner_type) => Some(inner_type.clone()),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        },
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            }
        },
        _ => None,
    }
}

fn get_type_within_vec(ty: &syn::Type) -> Option<syn::Type> {
    get_type_within(ty, "Vec")
}

fn get_type_within_option(ty: &syn::Type) -> Option<syn::Type> {
    get_type_within(ty, "Option")
}

fn get_fn_name(field: &syn::Field) -> Option<proc_macro2::Literal> {
    let attrs = field.attrs.clone();
    let fn_names: Vec<_> = attrs.into_iter().filter_map(|attr| {
        get_fn_name_from_attr(&attr)
    }).take(1).collect();

    if fn_names.is_empty() {
        None
    } else {
        Some(fn_names[0].clone())
    }
}

fn get_fn_name_from_attr(attr: &syn::Attribute) -> Option<proc_macro2::Literal> {
    use syn::Attribute;

    let Attribute { path, tokens, .. } = attr;

    match path.get_ident() {
        Some(ident) => {
            if ident.to_string() == "builder" {
                let tokens: proc_macro2::TokenStream = tokens.clone();
                let tokens: Vec<proc_macro2::TokenTree> = tokens.into_iter().collect();
                if tokens.len() != 1 {
                    None
                } else {
                    let tt = tokens[0].clone();
                    get_fn_name_from_token_tree(&tt)
                }
            } else {
                None
            }
        },
        _ => None,
    }
}

fn get_fn_name_from_token_tree(tt: &proc_macro2::TokenTree) -> Option<proc_macro2::Literal> {
    use proc_macro2::{TokenTree, TokenStream};

    if let TokenTree::Group(group) = tt {
        let ts: TokenStream = group.stream();
        let ts: Vec<TokenTree> = ts.into_iter().collect();
        if ts.len() != 3 {
            None
        } else {
            let token1 = ts[0].clone();
            let token2 = ts[1].clone();
            let token3 = ts[2].clone();

            if let TokenTree::Ident(inner_ident) = token1 {
                if inner_ident.to_string() == "each" {
                    if let TokenTree::Punct(punct) = token2 {
                        if punct.as_char() == '=' {
                            if let TokenTree::Literal(lit) = token3 {
                                Some(lit)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
    } else {
        None
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let builder_name = format_ident!("{}Builder", struct_name);

    let fields: Vec<Field> = match input.data {
        Data::Struct(struct_data) => {
            match struct_data.fields {
                Fields::Named(fields_named) => {
                    fields_named.named.into_iter().collect()
                },
                _ => panic!("this derive proc macro only supports structs with named fields"),
            }
        },
        _ => panic!("this derive proc macro only supports structs"),
    };

    let mut builder_fields = vec![];
    let mut builder_methods = vec![];
    let mut man_field_names = vec![]; // Mandatory fields
    let mut opt_field_names = vec![]; // Optional fields

    for field in fields.iter() {
        let field_type = field.ty.clone();
        let field_name = field.ident.clone().expect("expected field to have an ident");

        let user_defined_each_fn_name = get_fn_name(&field);
        let user_defined_each_fn_name_str: Option<String> = user_defined_each_fn_name.clone().map(|lit| {
            lit.to_string().chars().filter(|&c| c != '\"').collect()
        });
        // eprintln!("{:?}", field_name.to_string());
        let create_builder_method = user_defined_each_fn_name.is_none() || user_defined_each_fn_name_str != Some(field_name.to_string());

        if let Some(fn_name) = user_defined_each_fn_name_str {
            let fn_name = format_ident!("{}", fn_name);
            let inner_type = get_type_within_vec(&field_type).expect("Expected a type like Vec<...> for 'each = ...' attribute");
            let each_builder_method = quote! {
                fn #fn_name(&mut self, item: #inner_type) -> &mut Self {
                    self.#field_name.push(item);
                    self
                }
            };
            builder_methods.push(each_builder_method);
        }

        if !is_option_type(&field_type) {
            let builder_field = quote! {
                #field_name: Option<#field_type>
            };
            builder_fields.push(builder_field);

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };
            if create_builder_method {
                builder_methods.push(builder_method);
            }

            man_field_names.push(field_name);
        } else {
            let inner_type = get_type_within_option(&field_type).unwrap();
            let builder_field = quote! {
                #field_name: #field_type
            };
            builder_fields.push(builder_field);

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };
            if create_builder_method {
                builder_methods.push(builder_method);
            }

            opt_field_names.push(field_name);
        }
    }

    let output = quote! {
        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #builder_name {
            fn new() -> Self {
                Self {
                    #( #man_field_names: None, )*
                    #( #opt_field_names: None, )*
                }
            }

            #( #builder_methods )*
        }

        impl #builder_name {
            fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#man_field_names.is_none() {
                        let msg = format!("Missing field {}", stringify!(#man_field_names));
                        return Err(Box::from(msg))
                    }
                );*
                Ok(#struct_name {
                    #( #man_field_names: self.#man_field_names.clone().unwrap(), )*
                    #( #opt_field_names: self.#opt_field_names.clone(), )*
                })
            }
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name::new()
            }
        }
    };

    output.into()
}
