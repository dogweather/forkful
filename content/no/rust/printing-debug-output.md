---
title:                "Rust: Utskrift av feilutdata"
simple_title:         "Utskrift av feilutdata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å printe ut debug-utdata er en viktig del av enhver programmeringsprosess. Det lar deg se hvordan koden din fungerer, feilsøke eventuelle problemer og forbedre ytelsen. Derfor er det viktig å ha god forståelse av hvordan du gjør dette i Rust.

## Hvordan

Så hvordan printer du ut debug-utdata i Rust? La oss se på et enkelt eksempel:

```Rust 
fn main() {
    let num1 = 10;
    println!("Dette er verdien til num1: {}", num1);
}
```

I dette eksempelet bruker vi `println!` macroen til å printe ut verdien til variabelen `num1`. Macroer i Rust er en kraftig måte å generere kode på, og `println!` gjør det enkelt å printe ut tekst og verdier i konsollen.

Du kan også bruke `format!` macroen for å lage en tekststreng med verdier, og deretter printe denne.

```Rust
fn main() {
    let num1 = 10;
    let num2 = 5;
    let string = format!("Summen av num1 og num2 er {}", num1 + num2);

    println!("{}", string);
}
```

Dette vil printe ut: "Summen av num1 og num2 er 15".

Det er også mulig å bruke `eprint!` og `eprintln!` macroene for å printe ut debug-utdata til standard error i stedet for standard output.

## Deep Dive

Nå som vi har sett på noen eksempler på hvordan du kan printe ut debug-utdata i Rust, la oss dykke litt dypere inn i konseptet.

En viktig ting å huske på er at Rust ikke tillater deg å printe ut data som ikke implementerer `std::fmt::Display` traiten. Dette betyr at du ikke kan printe ut f.eks. en struct uten å først implementere dette traitet for den.

I tillegg til `println!` og `format!` macroene, har Rust også en `dbg!` macro som lar deg printe ut verdien til en variabel og samtidig returnere denne verdien. Dette kan være nyttig for debugging av komplekse kodebaser.

Du kan også bruke `dbg!` macroen til å printe ut informasjon om en variabel når du kjører koden med `cargo run --release`. Dette kan være nyttig for å sammenligne ytelsen til kode som kjøres i utviklingsmodus og produksjonsmodus.

## Se Også

- [Rust By Example: Formatting](https://doc.rust-lang.org/stable/rust-by-example/hello/print/fmt.html)
- [Rust Reference: Printing output](https://doc.rust-lang.org/reference/macros.html#printing)
- [Rust Cookbook: Debugging and Logging](https://rust-lang-nursery.github.io/rust-cookbook/development_tools/debugging.html)