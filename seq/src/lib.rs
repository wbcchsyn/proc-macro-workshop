use proc_macro::TokenStream;
use syn::parse::Parse;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _seq = syn::parse_macro_input!(input as Seq);

    TokenStream::new()
}

struct Seq {
    loop_var: syn::Ident,
    _in: syn::Token![in],
    start: syn::LitInt,
    dot2: syn::RangeLimits,
    end: syn::LitInt,
    block: Box<syn::Block>,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            loop_var: input.parse()?,
            _in: input.parse()?,
            start: input.parse()?,
            dot2: input.parse()?,
            end: input.parse()?,
            block: input.parse()?,
        })
    }
}
