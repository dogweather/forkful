---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolering av en streng i programmering refererer til prosessen med å sette inn variabler inn i en streng. Dette gjøres for å generere dynamisk innhold innenfor en streng på en ryddig og effektiv måte.

## Hvordan Gjøre Det:

```Rust
fn main() {
    let alder = 30;
    println!("Jeg er {} år gammel.", alder);
}
```
Utfall av ovennevnte kode vil være: `Jeg er 30 år gammel.`

Det er også mulig å interpolere flere variabler:

```Rust
fn main() {
    let navn = "Ola";
    let alder = 30;
    println!("Hei, jeg heter {} og jeg er {} år gammel.", navn, alder);
}
```
Utfall av denne koden vil være: `Hei, jeg heter Ola og jeg er 30 år gammel.`

## Grundigere Undersøkelse: 

Når det kommer til strenginterpolering, er det noen detaljer det er godt å være klar over. Historisk sett kommer interpolering av strenger fra Perl og har siden blitt adoptert av mange andre språk som Python, Ruby, og selvfølgelig Rust. I Rust gjøres dette ved hjelp av `println!`-makroen, som erstatter krøllparenteser `{}` med tilhørende variabler.

Alternativt, det er tilfeller der du kanskje vil formatere en streng uten å skrive den ut med en gang. I slike tilfeller kan du bruke `format!`-makroen:

```Rust
fn main() {
    let navn = "Ola";
    let alder = 30;
    let string = format!("Hei, jeg heter {} og jeg er {} år gammel.", navn, alder);
    println!("{}", string);
}
```
Programmet vil fortsatt skrive ut: `Hei, jeg heter Ola og jeg er 30 år gammel.`

## Se Også:

For mer informasjon om strenginterpolering i Rust, se følgende kilder:
- [Offisiell Rust dokumentasjon om `println!`](https://doc.rust-lang.org/std/macro.println.html)
- [Rust by Example: Formatering av utskrift](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- [Wikipedia: Strenginterpolering](https://en.wikipedia.org/wiki/String_interpolation)