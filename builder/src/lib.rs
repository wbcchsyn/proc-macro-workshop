use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn;

#[proc_macro_derive(Builder)]
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
    let dst_struct = dst_struct(&dst_struct_name, &src_fields);
    let src_builder_method = src_builder_method(src_struct_name, &dst_struct_name, &src_fields);
    let setter_methods = src_fields.iter().map(setter_method);

    Ok(quote! {
        #dst_struct

        #src_builder_method

        impl #dst_struct_name {
            #(#setter_methods)*
        }
    })
}

fn parse_fields(ast: &syn::DeriveInput) -> Result<Vec<syn::Field>, syn::Error> {
    let data = match &ast.data {
        syn::Data::Struct(data) => data,
        _ => {
            return Err(syn::Error::new_spanned(
                ast,
                "Builder derive only supports structs",
            ))
        }
    };

    Ok(data.fields.iter().map(Clone::clone).collect())
}

fn dst_struct_name(src_name: &syn::Ident) -> syn::Ident {
    let name = format!("__{}Builder__", src_name);
    syn::Ident::new(&name, src_name.span())
}

fn dst_struct(dst_name: &syn::Ident, src_fields: &[syn::Field]) -> TokenStream2 {
    let dst_fields = src_fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;

        quote! {
            #name: Option<#ty>,
        }
    });

    quote! {
        pub struct #dst_name {
            #(#dst_fields)*
        }
    }
}

fn src_builder_method(
    src_name: &syn::Ident,
    dst_name: &syn::Ident,
    src_fields: &[syn::Field],
) -> TokenStream2 {
    let dst_fileds = src_fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote! {
            #name: None,
        }
    });

    quote! {
        impl #src_name {
            pub fn builder() -> #dst_name {
                #dst_name {
                    #(#dst_fileds)*
                }
            }
        }
    }
}

fn setter_method(src_field: &syn::Field) -> TokenStream2 {
    let name = src_field.ident.as_ref().unwrap();
    let ty = &src_field.ty;

    quote! {
        pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = Some(#name);
            self
        }
    }
}
