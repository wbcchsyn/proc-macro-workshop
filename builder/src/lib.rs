use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    match do_derive(input) {
        Ok(output) => output.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn do_derive(input: TokenStream) -> Result<TokenStream2, syn::Error> {
    let ast: syn::DeriveInput = syn::parse(input)?;

    let src_struct_name = &ast.ident;
    let src_fields = parse_fields(&ast)?;

    let dst_struct_name = dst_struct_name(src_struct_name);
    let dst_struct_fields = src_fields.clone().map(dst_struct_field);
    let src_builder_method = src_builder_method(&dst_struct_name, src_fields.clone());
    let setter_methods = src_fields.clone().map(setter_method);
    let dst_build_method = dst_build_method(src_struct_name, src_fields.clone());

    Ok(quote! {
        pub struct #dst_struct_name {
            #(#dst_struct_fields)*
        }

        impl #src_struct_name {
            #src_builder_method
        }

        impl #dst_struct_name {
            #(#setter_methods)*

            #dst_build_method

            fn take_vec<T>(src: &mut Vec<T>) -> Vec<T> {
                let mut ret = Vec::new();
                ::std::mem::swap(src, &mut ret);
                ret
            }
        }
    })
}

fn is_same_path(lhs: &[&str], rhs: &syn::Path) -> bool {
    if rhs.leading_colon.is_some() && lhs.len() != rhs.segments.len() {
        return false;
    }
    if lhs.len() < rhs.segments.len() {
        return false;
    }
    if rhs.segments.len() == 0 {
        return false;
    }

    lhs.iter()
        .rev()
        .zip(rhs.segments.iter().rev())
        .all(|(&l, r)| r.ident == l)
}

fn is_option(ty: &syn::Type) -> bool {
    let path = match ty {
        syn::Type::Path(path) => &path.path,
        _ => return false,
    };

    is_same_path(&["std", "option", "Option"], path)
}

fn is_vec(ty: &syn::Type) -> bool {
    let path = match ty {
        syn::Type::Path(path) => &path.path,
        _ => return false,
    };

    is_same_path(&["std", "vec", "Vec"], path)
}

fn extract_last_template_parameter(ty: &syn::Type) -> TokenStream2 {
    let path = match ty {
        syn::Type::Path(path) => &path.path,
        _ => return TokenStream2::new(),
    };

    if path.segments.is_empty() {
        return TokenStream2::new();
    }

    match &path.segments.last().unwrap().arguments {
        syn::PathArguments::AngleBracketed(ret) => ret.args.to_token_stream(),
        syn::PathArguments::Parenthesized(ret) => ret.to_token_stream(),
        syn::PathArguments::None => TokenStream2::new(),
    }
}

fn parse_fields(
    ast: &syn::DeriveInput,
) -> Result<syn::punctuated::Iter<'_, syn::Field>, syn::Error> {
    let data = match &ast.data {
        syn::Data::Struct(data) => data,
        _ => {
            return Err(syn::Error::new_spanned(
                ast,
                "Builder derive only supports structs",
            ))
        }
    };

    Ok(data.fields.iter())
}

fn dst_struct_name(src_name: &syn::Ident) -> syn::Ident {
    let name = format!("__{}Builder__", src_name);
    syn::Ident::new(&name, src_name.span())
}

fn dst_struct_field(src_field: &syn::Field) -> TokenStream2 {
    let name = src_field.ident.as_ref().unwrap();
    let ty = &src_field.ty;

    if is_option(ty) {
        quote! {
            #name: #ty,
        }
    } else if is_vec(ty) {
        quote! {
            #name: #ty,
        }
    } else {
        quote! {
            #name: Option<#ty>,
        }
    }
}

fn src_builder_method<'a, T>(dst_name: &syn::Ident, src_fields: T) -> TokenStream2
where
    T: Iterator<Item = &'a syn::Field>,
{
    let dst_fileds = src_fields.map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote! {
            #name: Default::default(),
        }
    });

    quote! {
        pub fn builder() -> #dst_name {
            #dst_name {
                #(#dst_fileds)*
            }
        }
    }
}

fn builder_each_attributes(src_field: &syn::Field) -> syn::Result<Vec<syn::Ident>> {
    let attrs = src_field.attrs.iter();
    let attrs = attrs.filter(|&attr| attr.path().is_ident("builder"));

    let mut ret = Vec::new();

    for attr in attrs {
        let meta: syn::Meta = attr.parse_args()?;
        match meta {
            syn::Meta::NameValue(mnv) => {
                if mnv.path.is_ident("each") {
                    match &mnv.value {
                        syn::Expr::Lit(lit) => match &lit.lit {
                            syn::Lit::Str(lit) => ret.push(lit.parse()?),
                            _ => {
                                return Err(syn::Error::new_spanned(
                                    mnv.value,
                                    "expected string literal",
                                ))
                            }
                        },
                        _ => {
                            return Err(syn::Error::new_spanned(
                                mnv.value,
                                "expected string literal",
                            ))
                        }
                    }
                } else {
                    return Err(syn::Error::new_spanned(
                        mnv.path,
                        "expected `builder(each = \"...\")`",
                    ));
                }
            }
            _ => {
                return Err(syn::Error::new_spanned(
                    meta,
                    "expected `builder(each = \"...\")`",
                ))
            }
        }
    }

    Ok(ret)
}

fn setter_method(src_field: &syn::Field) -> TokenStream2 {
    let name = src_field.ident.as_ref().unwrap();
    let ty = &src_field.ty;

    let eachs = match builder_each_attributes(src_field) {
        Ok(eachs) => eachs,
        Err(err) => return err.to_compile_error(),
    };

    if 0 < eachs.len() {
        if !is_vec(ty) {
            return syn::Error::new_spanned(
                src_field,
                "`builder` attribute supports only `Vec` type property.",
            )
            .to_compile_error();
        }

        let each_setters = eachs.iter().map(|each| {
            let each = each.to_token_stream();
            let ty = extract_last_template_parameter(ty);

            quote! {
                pub fn #each(&mut self, val: #ty) -> &mut Self {
                    self.#name.push(val);
                    self
                }
            }
        });

        return quote! {
            #(#each_setters)*
        };
    }

    if is_option(ty) {
        let ty = extract_last_template_parameter(ty);

        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    } else if is_vec(ty) {
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = #name;
                self
            }
        }
    } else {
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name);
                self
            }
        }
    }
}

fn dst_build_method<'a, T>(src_name: &syn::Ident, src_fields: T) -> TokenStream2
where
    T: Iterator<Item = &'a syn::Field> + Clone,
{
    let built_check = src_fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        if is_option(ty) || is_vec(ty) {
            TokenStream2::new()
        } else {
            quote! {
                if self.#name.is_none() {
                    let msg = format!("Field {} is not set yet.", stringify!(#name));
                    return Err(msg.into());
                }
            }
        }
    });

    let built_fields = src_fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        if is_option(ty) {
            quote! {
                #name: self.#name.take(),
            }
        } else if is_vec(ty) {
            quote! {
                #name: Self::take_vec(&mut self.#name),
            }
        } else {
            quote! {
                #name: self.#name.take().unwrap(),
            }
        }
    });

    quote! {
        pub fn build(&mut self) -> ::std::result::Result<#src_name, ::std::boxed::Box<dyn ::std::error::Error>> {
            #(#built_check)*

            ::std::result::Result::Ok(#src_name {
                #(#built_fields)*
            })
        }
    }
}
