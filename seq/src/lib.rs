use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, TokenTree as TokenTree2};
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
    if contains_loop(&seq.block) {
        Ok(expand_loop(&seq.block, start, end, &variable))
    } else {
        let mut rev_tokens = seq.block;
        rev_tokens.reverse();

        let acc = (start..end).fold(Vec::new(), |mut acc, value| {
            acc.push(expand_variable(rev_tokens.clone(), &variable, value));
            acc
        });

        Ok(quote! { #(#acc)* })
    }
}

fn expand_loop(mut tokens: &[TokenTree2], start: i128, end: i128, variable: &str) -> TokenStream2 {
    let mut acc = Vec::new();

    while 0 < tokens.len() {
        if 2 < tokens.len() && is_loop(&tokens[0], &tokens[1], &tokens[2]) {
            let group = match &tokens[1] {
                TokenTree2::Group(group) => group,
                _ => unreachable!(),
            };

            let mut rev_tokens: Vec<TokenTree2> = group.stream().into_iter().collect();
            rev_tokens.reverse();

            (start..end)
                .for_each(|value| acc.push(expand_variable(rev_tokens.clone(), variable, value)));
            tokens = &tokens[3..];
        } else if let TokenTree2::Group(group) = &tokens[0] {
            let inner_tokens: Vec<TokenTree2> = group.stream().into_iter().collect();
            let inner_tokens = expand_loop(&inner_tokens, start, end, variable);

            use proc_macro2::Delimiter;
            match group.delimiter() {
                Delimiter::Parenthesis => acc.push(quote! { (#inner_tokens) }),
                Delimiter::Brace => acc.push(quote! { {#inner_tokens} }),
                Delimiter::Bracket => acc.push(quote! { [#inner_tokens] }),
                Delimiter::None => acc.push(inner_tokens),
            }

            tokens = &tokens[1..];
        } else {
            acc.push(tokens[0].to_token_stream());
            tokens = &tokens[1..];
        }
    }

    quote! {#(#acc)*}
}

fn expand_variable(mut rev_tokens: Vec<TokenTree2>, variable: &str, value: i128) -> TokenStream2 {
    let mut acc = Vec::new();

    while !rev_tokens.is_empty() {
        match rev_tokens.last().unwrap() {
            TokenTree2::Group(group) => {
                use proc_macro2::Delimiter;
                let mut rev_inner_tokens: Vec<TokenTree2> = group.stream().into_iter().collect();
                rev_inner_tokens.reverse();

                let inner = expand_variable(rev_inner_tokens, variable, value);
                match group.delimiter() {
                    Delimiter::Parenthesis => acc.push(quote! { (#inner) }),
                    Delimiter::Brace => acc.push(quote! { {#inner} }),
                    Delimiter::Bracket => acc.push(quote! { [#inner] }),
                    Delimiter::None => acc.push(inner),
                }

                rev_tokens.pop();
            }
            TokenTree2::Ident(_) => {
                if expand_tilda_pattern(&mut rev_tokens, variable, value) {
                    // Do nothing, because the expanded token is already pushed to `rev_tokens`.
                    // Go to next iteration.
                } else {
                    let ident = match rev_tokens.pop().unwrap() {
                        TokenTree2::Ident(ident) => ident,
                        _ => unreachable!(),
                    };

                    if ident == variable {
                        let token = syn::LitInt::new(&value.to_string(), ident.span());
                        acc.push(token.into_token_stream());
                    } else {
                        acc.push(ident.into_token_stream());
                    }
                }
            }
            _ => {
                let token = rev_tokens.pop().unwrap();
                acc.push(token.into_token_stream());
            }
        }
    }

    quote! { #(#acc)* }
}

fn contains_loop(tokens: &[TokenTree2]) -> bool {
    if tokens.windows(3).any(|tokens| {
        let (sharp, group, asterisk) = (&tokens[0], &tokens[1], &tokens[2]);
        is_loop(sharp, group, asterisk)
    }) {
        return true;
    }

    tokens
        .iter()
        .filter_map(|t| match t {
            TokenTree2::Group(group) => Some(group),
            _ => None,
        })
        .any(|group| {
            let tokens: Vec<TokenTree2> = group.stream().into_iter().collect();
            contains_loop(&tokens)
        })
}

fn is_loop(sharp: &TokenTree2, group: &TokenTree2, asterisk: &TokenTree2) -> bool {
    match sharp {
        TokenTree2::Punct(punct) if punct.as_char() == '#' => {}
        _ => return false,
    };

    match group {
        TokenTree2::Group(group) if group.delimiter() == proc_macro2::Delimiter::Parenthesis => {}
        _ => return false,
    }

    match asterisk {
        TokenTree2::Punct(punct) if punct.as_char() == '*' => {}
        _ => return false,
    }

    true
}

/// If the tokens starts with (`rev_tokens` ends with) a tilde pattern like `prefix~N` or `prefix~N~suffix`, consumes them,
/// expands into an `Ident`, pushes it to the `rev_tokens`, and returns true; otherwise, returns false.
fn expand_tilda_pattern(rev_tokens: &mut Vec<TokenTree2>, variable: &str, value: i128) -> bool {
    // tilda pattern requires at least 3 tokens.
    if rev_tokens.len() < 3 {
        return false;
    }

    // The first token must be `syn::Ident`
    let prefix = match rev_tokens.last().unwrap() {
        TokenTree2::Ident(ident) => ident,
        _ => return false,
    };

    // The second token must be '~'.
    match &rev_tokens[rev_tokens.len() - 2] {
        TokenTree2::Punct(punct) if punct.as_char() == '~' => {}
        _ => return false,
    };

    // The third token must be variable.
    match &rev_tokens[rev_tokens.len() - 3] {
        TokenTree2::Ident(ident) if ident == variable => {}
        _ => return false,
    };

    let expanded = syn::Ident::new(&format!("{}{}", prefix, value), prefix.span());
    (0..3).for_each(|_| {
        rev_tokens.pop();
    });

    // If '~suffix' follows, append the suffix to expanded.
    if rev_tokens.len() < 2 {
        rev_tokens.push(TokenTree2::Ident(expanded));
        return true;
    }

    // The first token must be '~'.
    match rev_tokens.last().unwrap() {
        TokenTree2::Punct(punct) if punct.as_char() == '~' => {}
        _ => {
            rev_tokens.push(TokenTree2::Ident(expanded));
            return true;
        }
    }

    // The second token must be `Ident`.
    let ident = match rev_tokens[rev_tokens.len() - 2] {
        TokenTree2::Ident(ref ident) => ident,
        _ => {
            rev_tokens.push(TokenTree2::Ident(expanded));
            return true;
        }
    };

    let expanded = syn::Ident::new(&format!("{}{}", expanded, ident), expanded.span());
    (0..2).for_each(|_| {
        rev_tokens.pop();
    });
    rev_tokens.push(TokenTree2::Ident(expanded));

    true
}

struct Seq {
    loop_var: syn::Ident,
    _in: syn::Token![in],
    start: syn::LitInt,
    dot2: syn::RangeLimits,
    end: syn::LitInt,
    _brace: syn::token::Brace,
    block: Vec<TokenTree2>,
}

impl Parse for Seq {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let loop_var: syn::Ident = input.parse()?;
        let _in: syn::Token![in] = input.parse()?;
        let start: syn::LitInt = input.parse()?;
        let dot2: syn::RangeLimits = input.parse()?;
        let end: syn::LitInt = input.parse()?;

        let content;
        let _brace = syn::braced!(content in input);

        let mut block: Vec<TokenTree2> = Vec::new();
        while !content.is_empty() {
            block.push(content.parse()?);
        }

        Ok(Self {
            loop_var,
            _in,
            start,
            dot2,
            end,
            _brace,
            block,
        })
    }
}
