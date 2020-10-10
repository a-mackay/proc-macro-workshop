extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Data, Fields, Field};

fn is_option_type(ty: &syn::Type) -> bool {
    get_type_within_option(ty).is_some()
}

fn is_vec_type(ty: &syn::Type) -> bool {
    get_type_within_vec(ty).is_some()
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

fn get_fn_name(field: &syn::Field) -> Result<Option<proc_macro2::Literal>, String> {
    let attrs = field.attrs.clone();
    let fn_names: Result<Vec<Option<_>>, String> = attrs.into_iter().map(|attr| {
        get_fn_name_from_attr(&attr)
    }).take(1).collect();

    let fn_names: Vec<Option<_>> = fn_names?;
    let fn_names: Vec<proc_macro2::Literal> = fn_names.into_iter().filter_map(|opt| opt).collect();

    if fn_names.is_empty() {
        Ok(None)
    } else {
        Ok(Some(fn_names[0].clone()))
    }
}

fn get_fn_name_from_attr(attr: &syn::Attribute) -> Result<Option<proc_macro2::Literal>, String> {
    use syn::Attribute;

    let Attribute { path, tokens, .. } = attr;

    match path.get_ident() {
        Some(ident) => {
            if ident.to_string() == "builder" {
                let tokens: proc_macro2::TokenStream = tokens.clone();
                let tokens: Vec<proc_macro2::TokenTree> = tokens.into_iter().collect();
                if tokens.len() != 1 {
                    Err("expected one TokenTree in builder attribute".into())
                } else {
                    let tt = tokens[0].clone();
                    get_fn_name_from_token_tree(&tt).map(|lit| Some(lit))
                }
            } else {
                Ok(None)
            }
        },
        _ => Ok(None),
    }
}

fn get_fn_name_from_token_tree(tt: &proc_macro2::TokenTree) -> Result<proc_macro2::Literal, String> {
    use proc_macro2::{TokenTree, TokenStream};

    if let TokenTree::Group(group) = tt {
        let ts: TokenStream = group.stream();
        let ts: Vec<TokenTree> = ts.into_iter().collect();
        if ts.len() != 3 {
            Err("builder attribute expected exactly 3 tokens (each, =, \"...\")".into())
        } else {
            let token1 = ts[0].clone();
            let token2 = ts[1].clone();
            let token3 = ts[2].clone();

            if let TokenTree::Ident(inner_ident) = token1 {
                if inner_ident.to_string() == "each" {
                    if let TokenTree::Punct(punct) = token2 {
                        if punct.as_char() == '=' {
                            if let TokenTree::Literal(lit) = token3 {
                                Ok(lit)
                            } else {
                                Err("builder attribute 3rd token should be a string literal".into())
                            }
                        } else {
                            Err("builder attribute 2nd token should be '='".into())
                        }
                    } else {
                        Err("builder attribute 2nd token should be TokenTree::Punct '='".into())
                    }
                } else {
                    Err("expected `builder(each = \"...\")`".into())
                }
            } else {
                Err("builder attribute 1st token should be TokenTree::Ident 'each'".into())
            }
        }
    } else {
        Err("builder attribute should be a TokenTree::Group".into())
    }
}

#[derive(Clone, Debug)]
enum FieldType {
    Normal,
    Vec,
    Option,
}

struct CodeFeatures {
    field_name: syn::Ident,
    builder_field: proc_macro2::TokenStream,
    builder_method: proc_macro2::TokenStream,
    field_type: FieldType,
    create_builder_method: bool,
    each_builder_method: Option<proc_macro2::TokenStream>,
}

fn decide_field_type(ty: &syn::Type) -> FieldType {
    let results = (is_option_type(ty), is_vec_type(ty));
    match results {
        (true, false) => FieldType::Option,
        (false, true) => FieldType::Vec,
        (false, false) => FieldType::Normal,
        (true, true) => unreachable!(),
    }
}

fn field_to_code_features(field: &syn::Field) -> Result<CodeFeatures, String> {
    let field_type = field.ty.clone();
    let field_name = field.ident.clone().expect("expected field to have an ident");

    let user_defined_each_fn_name = get_fn_name(&field)?;
    let user_defined_each_fn_name_str: Option<String> = user_defined_each_fn_name.clone().map(|lit| {
        lit.to_string().chars().filter(|&c| c != '\"').collect()
    });
    let create_builder_method = user_defined_each_fn_name.is_none() || user_defined_each_fn_name_str != Some(field_name.to_string());

    let each_builder_method = match user_defined_each_fn_name_str {
        Some(fn_name) => {
            let fn_name = format_ident!("{}", fn_name);
            let inner_type = get_type_within_vec(&field_type).expect("Expected a type like Vec<...> for 'each = ...' attribute");
            Some(quote! {
                fn #fn_name(&mut self, item: #inner_type) -> &mut Self {
                    let items = self.#field_name.get_or_insert(vec![]);
                    items.push(item);
                    self
                }
            })
        },
        _ => None,
    };

    let decided_field_type = decide_field_type(&field_type);

    let code_features = match decided_field_type {
        FieldType::Normal => {
            let builder_field = quote! {
                #field_name: Option<#field_type>
            };

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };

            CodeFeatures {
                field_name,
                builder_field,
                builder_method,
                field_type: decided_field_type,
                create_builder_method,
                each_builder_method,
            }
        },
        FieldType::Option => {
            let inner_type = get_type_within_option(&field_type).unwrap();
            let builder_field = quote! {
                #field_name: #field_type
            };

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #inner_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };

            CodeFeatures {
                field_name,
                builder_field,
                builder_method,
                field_type: decided_field_type,
                create_builder_method,
                each_builder_method,
            }
        },
        FieldType::Vec => {
            let builder_field = quote! {
                #field_name: Option<#field_type>
            };

            let builder_method = quote! {
                fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                    self.#field_name = Some(#field_name);
                    self
                }
            };

            CodeFeatures {
                field_name,
                builder_field,
                builder_method,
                field_type: decided_field_type,
                create_builder_method,
                each_builder_method,
            }
        },
    };

    Ok(code_features)
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
    let mut normal_field_names = vec![]; // Regular fields
    let mut opt_field_names = vec![]; // `Option` fields
    let mut vec_field_names = vec![]; // `Vec` fields

    let mut macro_errs = vec![];

    for field in fields.iter() {
        match field_to_code_features(field) {
            Ok(CodeFeatures { field_name, builder_field, builder_method, field_type, create_builder_method, each_builder_method }) => {
                match field_type {
                    FieldType::Normal => normal_field_names.push(field_name),
                    FieldType::Option => opt_field_names.push(field_name),
                    FieldType::Vec => vec_field_names.push(field_name),
                };

                if let Some(each_builder_method) = each_builder_method {
                    builder_methods.push(each_builder_method);
                }

                builder_fields.push(builder_field);
                if create_builder_method {
                    builder_methods.push(builder_method);
                }
            },
            Err(e) => {
                let err = quote! {
                    compile_error!(#e)
                };
                macro_errs.push(err)
            }
        }
    }

    let output = quote! {
        #( #macro_errs; )*

        pub struct #builder_name {
            #(#builder_fields,)*
        }

        impl #builder_name {
            fn new() -> Self {
                Self {
                    #( #normal_field_names: None, )*
                    #( #opt_field_names: None, )*
                    #( #vec_field_names: None, )*
                }
            }

            #( #builder_methods )*
        }

        fn optvec_to_vec<T>(optvec: Option<Vec<T>>) -> Vec<T> {
            match optvec {
                Some(vec) => vec,
                None => vec![],
            }
        }

        impl #builder_name {
            fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    if self.#normal_field_names.is_none() {
                        let msg = format!("Missing field {}", stringify!(#normal_field_names));
                        return Err(Box::from(msg))
                    }
                );*
                Ok(#struct_name {
                    #( #normal_field_names: self.#normal_field_names.clone().unwrap(), )*
                    #( #opt_field_names: self.#opt_field_names.clone(), )*
                    #( #vec_field_names: optvec_to_vec(self.#vec_field_names.clone()), )*
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
