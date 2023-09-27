use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn;
use syn::visit_mut::VisitMut;

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
    let item = match item {
        syn::Item::Enum(item) => item,
        _ => {
            return Err(syn::Error::new_spanned(
                args,
                "expected enum or match expression",
            ))
        }
    };

    for i in 1..item.variants.len() {
        let prev = &item.variants[i - 1].ident;
        let cur = &item.variants[i].ident;

        if cur < prev {
            for j in 0..i {
                let prev = &item.variants[j].ident;
                let cur = &item.variants[i].ident;

                if cur < prev {
                    return Err(syn::Error::new_spanned(
                        cur,
                        format!("{} should sort before {}", cur, prev),
                    ));
                }
            }
        }
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let args: TokenStream2 = args.into();
    let mut item = syn::parse_macro_input!(input as syn::ItemFn);

    let err = match do_check(&args, &mut item) {
        Ok(()) => TokenStream2::new(),
        Err(err) => err.to_compile_error(),
    };

    MatchVisitor.visit_item_fn_mut(&mut item);
    quote! {
        #item
        #err
    }
    .into()
}

struct MatchVisitor;

impl syn::visit_mut::VisitMut for MatchVisitor {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        node.attrs.retain(|attr| !attr.path().is_ident("sorted"));
        syn::visit_mut::visit_expr_match_mut(self, node);
    }
}

fn do_check(_args: &TokenStream2, item: &mut syn::ItemFn) -> syn::Result<()> {
    for stmt in item.block.stmts.iter() {
        if let syn::Stmt::Expr(syn::Expr::Match(em), _) = stmt {
            do_check_match(em)?;
        }
    }

    Ok(())
}

fn do_check_match(item: &syn::ExprMatch) -> syn::Result<()> {
    // Do nothing if #[sorted] is not present.
    if !item.attrs.iter().any(|attr| attr.path().is_ident("sorted")) {
        return Ok(());
    }

    // Check that the match arms are sorted.
    for i in 1..item.arms.len() {
        let prev = item.arms[i - 1].pat.to_token_stream().to_string();
        let cur = item.arms[i].pat.to_token_stream().to_string();

        if cur < prev {
            for j in 0..i {
                let bad = item.arms[j].pat.to_token_stream().to_string();
                if cur < bad {
                    let cur = &item.arms[i].pat;
                    let bad = &item.arms[j].pat;
                    return Err(syn::Error::new_spanned(
                        pat_to_token(cur),
                        format!("{} should sort before {}", show_pat(cur), show_pat(bad),),
                    ));
                }
            }
        }
    }

    Ok(())
}

/// I don't think this method is really necessary, but the prepared test seems to adopt this logic.
fn pat_to_token(pat: &syn::Pat) -> TokenStream2 {
    match pat {
        syn::Pat::Path(path) => path.path.to_token_stream(),
        syn::Pat::TupleStruct(tpl) => tpl.path.to_token_stream(),
        syn::Pat::Struct(st) => st.path.to_token_stream(),
        _ => pat.to_token_stream(),
    }
}

fn show_path(path: &syn::Path) -> String {
    let segments: Vec<String> = path
        .segments
        .iter()
        .map(|seg| seg.ident.to_string())
        .collect();
    match &path.leading_colon {
        Some(_) => format!("::{}", segments.join("::")),
        None => segments.join("::"),
    }
}

fn show_pat(pat: &syn::Pat) -> String {
    match pat {
        syn::Pat::Path(path) => show_path(&path.path),
        syn::Pat::TupleStruct(tpl) => show_path(&tpl.path),
        syn::Pat::Struct(st) => show_path(&st.path),
        _ => pat.to_token_stream().to_string(),
    }
}
