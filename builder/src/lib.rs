extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Field};

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

    let mut builder_fields = vec![];
    let mut builder_methods = vec![];
    let mut field_names = vec![];

    for field in fields.iter() {
        let field_type = field.ty.clone();
        let field_name = field.ident.clone().expect("expected field to have an ident");

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
        builder_methods.push(builder_method);

        field_names.push(field_name);
    }

    let output = quote! {
        pub struct #builder_name {
            #(#builder_fields),*
        }

        impl #builder_name {
            fn new() -> Self {
                Self {
                    #( #field_names: None ),*
                }
            }

            #(#builder_methods)*
        }

        impl #builder_name {
            fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#field_names.is_none() {
                        let msg = format!("Missing field {}", stringify!(#field_names));
                        return Err(Box::from(msg))
                    }
                );*
                Ok(#struct_name {
                    #( #field_names: self.#field_names.clone().unwrap() ),*
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
