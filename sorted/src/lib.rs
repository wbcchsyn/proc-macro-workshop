use proc_macro::TokenStream;
use syn;
use quote::ToTokens;

#[proc_macro_attribute]
pub fn sorted(_: TokenStream, input: TokenStream) -> TokenStream {
    let item = syn::parse_macro_input!(input as syn::Item);
    item.into_token_stream().into()
}
