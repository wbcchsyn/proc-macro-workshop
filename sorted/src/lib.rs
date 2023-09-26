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
