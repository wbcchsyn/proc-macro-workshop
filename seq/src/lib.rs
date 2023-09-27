use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::parse::Parse;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = syn::parse_macro_input!(input as Seq);

    match do_seq(seq) {
        Ok(token) => token.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn do_seq(seq: Seq) -> syn::Result<TokenStream2> {
    let start: i128 = seq.start.base10_parse()?;
    let end: i128 = {
        let end = seq.end.base10_parse()?;
        match seq.dot2 {
            syn::RangeLimits::HalfOpen(_) => end,
            syn::RangeLimits::Closed(_) => end + 1,
        }
    };

    let variable = seq.loop_var.to_string();
    let tokens = (start..end).fold(Vec::new(), |mut acc, i| {
        for stmt in seq.block.stmts.iter() {
            let token = replace_ident(stmt, &variable, i);
            acc.push(token);
        }
        acc
    });

    Ok(quote! { #(#tokens)* })
}

fn replace_ident<T: ToTokens>(token: &T, variable: &str, value: i128) -> TokenStream2 {
    let tokens = token.to_token_stream().into_iter().map(|tree| match &tree {
        proc_macro2::TokenTree::Ident(ident) => {
            if ident == variable {
                syn::LitInt::new(&value.to_string(), ident.span()).into_token_stream()
            } else {
                tree.into_token_stream()
            }
        }
        proc_macro2::TokenTree::Group(group) => {
            let inner = replace_ident(&group.stream(), variable, value);
            match group.delimiter() {
                proc_macro2::Delimiter::Parenthesis => quote! { (#inner) },
                proc_macro2::Delimiter::Brace => quote! { {#inner} },
                proc_macro2::Delimiter::Bracket => quote! { <#inner> },
                proc_macro2::Delimiter::None => inner,
            }
        }
        _ => tree.into_token_stream(),
    });

    quote! { #(#tokens)* }
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
