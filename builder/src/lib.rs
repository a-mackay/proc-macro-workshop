extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Field};

fn is_option_type(ty: &syn::Type) -> bool {
    get_type_within_option(ty).is_some()
}

fn get_type_within_option(ty: &syn::Type) -> Option<syn::Type> {
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
                if ident.to_string() == "Option" {
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

#[proc_macro_derive(Builder)]
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

    let mut man_builder_fields = vec![]; // Mandatory fields
    let mut opt_builder_fields = vec![]; // Optional fields
    let mut man_builder_methods = vec![];
    let mut opt_builder_methods = vec![];
    let mut man_field_names = vec![];
    let mut opt_field_names = vec![];

    for field in fields.iter() {
        let field_type = field.ty.clone();
        let field_name = field.ident.clone().expect("expected field to have an ident");

        if !is_option_type(&field_type) {
            let builder_field = quote! {
                #field_name: Option<#field_type>
            };
            man_builder_fields.push(builder_field);

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };
            man_builder_methods.push(builder_method);

            man_field_names.push(field_name);
        } else {
            let inner_type = get_type_within_option(&field_type).unwrap();
            let builder_field = quote! {
                #field_name: #field_type
            };
            opt_builder_fields.push(builder_field);

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };
            opt_builder_methods.push(builder_method);

            opt_field_names.push(field_name);
        }
    }

    let output = quote! {
        pub struct #builder_name {
            #(#man_builder_fields,)*
            #(#opt_builder_fields,)*
        }

        impl #builder_name {
            fn new() -> Self {
                Self {
                    #( #man_field_names: None, )*
                    #( #opt_field_names: None, )*
                }
            }

            #(#man_builder_methods)*
            #(#opt_builder_methods)*
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
