---
title:                "Rust: Sammenslåing av strenger"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger er en viktig del av programmering, spesielt i Rust. Dette er fordi Rust bruker statisk typering, noe som betyr at typen til en variabel må være kjent på kompileringstidspunktet. Ved å kombinere strenger på riktig måte, kan vi unngå uønskede feil og sikre at programmet vårt fungerer som det skal.

## Hvordan gjøre det

I Rust kan vi kombinere strenger på to forskjellige måter: ved hjelp av "format!" - makroen eller ved å bruke "+ =" - operatøren. La oss se et eksempel på begge deler:

```Rust
// Med "format!" - makroen
let navn = "Ole";
let alder = 22;
let intro = format!("Hei, mitt navn er {} og jeg er {} år gammel.", navn, alder);
println!("{}", intro);

// Med "+ =" - operatøren
let navn = "Kari";
let yrke = "webutvikler";
let info = "Jeg er";
let mut setning = String::from(info); 
setning += " " + navn + " " + yrke + ".";
println!("{}", setning);
```

Output for begge deler vil være:

```
Hei, mitt navn er Ole og jeg er 22 år gammel.
Jeg er Kari webutvikler.
```

Som du kan se, bruker "format!" - makroen måten til å formatere en string på en enkel og leselig måte, mens "+ =" - operatøren gir oss mer manuell kontroll over hvordan vi kombinerer strenger.

## Dykk dypere

Når vi bruker "format!" - makroen, kan vi også inkludere variabler ved hjelp av " {} " - tegnene. Dette er nyttig når vi vil legge til tall i en string. Se på dette eksempelet:

```Rust
let pris = 299;
let produkt = "sko";
let setning = format!("Prisen på {} er {} NOK.", produkt, pris);
println!("{}", setning);
```

Output vil være:

```
Prisen på sko er 299 NOK.
```

Vi kan også endre rekkefølgen på variablene ved å inkludere et nummer inne i " {} " - tegnene. Dette er nyttig hvis vi for eksempel trenger å bytte mellom å angi navn og alder:

```Rust
let navn = "Marianne";
let alder = 35;
let setning = format!("Jeg heter {} og jeg er {} år gammel.", navn, alder);
println!("{}", setning);
```

Output vil være:

```
Jeg heter Marianne og jeg er 35 år gammel.
```

## Se også

For en mer detaljert oversikt over hvordan du kan kombinere strenger i Rust, kan du se følgende ressurser:

- [Offisiell Rust dokumentasjon om strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust by Example - strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [How to Concatenate Strings in Rust](https://www.techiedelight.com/concatenate-strings-rust/)