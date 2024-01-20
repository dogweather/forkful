---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av feilsøkingsdata er prosessen med å vise programdata for å løse problemer. Programmerere bruker det for å forstå hva som skjer under panseret når noe går galt.

## Hvordan gjør jeg det:
```Rust
fn main() {
    let data = vec![1, 2, 3];
    println!("{:?}", data);
}
```
Dette vil skrive ut: `[1, 2, 3]`

Hvis datatypen din ikke støtter debug-utskrift, kan du legge til en `#[derive(Debug)]` annotering:

```Rust
#[derive(Debug)]
struct Person {
    name: String,
    age: u8,
}

fn main() {
    let person = Person { name: String::from("Alice"), age: 20 };
    println!("{:#?}", person);
}
```
Utskriften vil være:

```
Person {
    name: "Alice",
    age: 20,
}
```

## Dypt Dykk

Historisk sett, oppsto behovet for feilsøkingsutskrift som en enkel metode for å forstå hva en kode gjør, spesielt når du finner feil. Det har vært i bruk siden tidlige programmeringsspråk.

Alternativene inkluderer logger, som gir mer kontroll over hvor og hvordan de skriver ut. Du kan også bruke en debugger, som lar deg sjekke programtilstanden på ethvert punkt.

Når det gjelder implementeringsdetaljer, gir Rust deg `fmt::Debug`-trait som kan brukes til å skrive ut din datatypes tilstand. Et annet nyttig verktøy er `#[derive(Debug)]`, som genererer en grunnleggende implementasjon for deg.

## Se også

- [Rust sin Debug Trait Dokumentasjon](https://doc.rust-lang.org/std/fmt/#formatting-traits)
- [Rust sin Kunnskapsbase om Utledning](https://doc.rust-lang.org/rust-by-example/trait/derive.html)
- [Bloggpost om Praktisk Feilsøking i Rust](https://medium.com/nearprotocol/debugging-rust-a537424a2609)