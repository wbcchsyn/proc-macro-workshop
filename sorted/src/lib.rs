use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let args: TokenStream2 = args.into();
    let item = syn::parse_macro_input!(input as syn::Item);

    let err = match do_sorted(&args, &item) {
        Ok(()) => TokenStream2::new(),
        Err(err) => err.to_compile_error(),
    };

    quote! {
        #item
        #err
    }
    .into()
}

fn do_sorted(args: &TokenStream2, item: &syn::Item) -> syn::Result<()> {
    match item {
        syn::Item::Enum(_) => Ok(()),
        _ => Err(syn::Error::new_spanned(
            args,
            "expected enum or match expression",
        )),
    }
}
