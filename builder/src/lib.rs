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
    let dst_struct = dst_struct(&dst_struct_name, src_fields.clone());
    let src_builder_method = src_builder_method(&dst_struct_name, src_fields.clone());
    let setter_methods = src_fields.clone().map(setter_method);
    let dst_build_method = dst_build_method(src_struct_name, src_fields.clone());

    Ok(quote! {
        #dst_struct

        impl #src_struct_name {
            #src_builder_method
        }

        impl #dst_struct_name {
            #(#setter_methods)*

            #dst_build_method
        }
    })
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

fn dst_struct<'a, T>(dst_name: &syn::Ident, src_fields: T) -> TokenStream2
where
    T: Iterator<Item = &'a syn::Field>,
{
    let dst_fields = src_fields.map(|field| {
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

fn src_builder_method<'a, T>(dst_name: &syn::Ident, src_fields: T) -> TokenStream2
where
    T: Iterator<Item = &'a syn::Field>,
{
    let dst_fileds = src_fields.map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote! {
            #name: None,
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

fn dst_build_method<'a, T>(src_name: &syn::Ident, src_fields: T) -> TokenStream2
where
    T: Iterator<Item = &'a syn::Field> + Clone,
{
    let built_check = src_fields.clone().map(|field|{
        let name = field.ident.as_ref().unwrap();
        quote! {
            if self.#name.is_none() {
                let msg = format!("Field {} is not set yet.", stringify!(#name));
                return Err(msg.into());
            }
        }
    });

    let built_fields = src_fields.clone().map(|field| {
        let name = field.ident.as_ref().unwrap();
        quote! {
            #name: self.#name.take().unwrap(),
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
