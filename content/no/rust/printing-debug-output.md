---
title:                "Skriving av feilsøkningsutdata"
html_title:           "Rust: Skriving av feilsøkningsutdata"
simple_title:         "Skriving av feilsøkningsutdata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å skrive ut feilsøkningsutdata er en viktig del av utviklingsprosessen for programvare. Det handler om å legge inn kode som skriver ut informasjon som hjelper deg med å forstå hva som foregår i programmet ditt mens det kjører. Dette er nyttig for å finne og fikse feil og for å forstå hvordan koden din fungerer.

## Hvordan:

```Rust
fn main() {
    println!("Hei fra Rust!");
}
```

Dette eksempelet viser hvordan du bruker `println!` -makroen i Rust til å skrive ut en streng til konsollen. Du kan også skrive ut variabler og andre verdier ved å inkludere dem i `println!` -kallet med en komma-separert liste. For eksempel:

```Rust
let navn = "Jonas";
let alder = 25;
println!("Hei, mitt navn er {} og jeg er {} år gammel.", navn, alder);
```

Output vil da være: `Hei, mitt navn er Jonas og jeg er 25 år gammel.`

## Dypdykk:

Historisk sett har debug-utskrift vært en viktig del av programmering for å finne og løse feil i komplekse programmer. I tillegg til å bruke `println!` -makroen, kan du også bruke Rusts `println_dbg!` -makro for å få mer detaljert informasjon om koden din når du feilsøker.

Det finnes også alternativer til å skrive ut feilsøkningsutdata, for eksempel å bruke en debuggere eller logger-biblioteker som `log` eller `slog`. Disse gir mer avanserte funksjoner for å håndtere og analysere utdata.

Når det gjelder implementasjonsdetaljer, bruker Rusts `println!` og `println_dbg!` -makroen `fmt` -modulen for formatering av utdata. Denne modulen gir mange nyttige funksjoner for å formatere utdata på en oversiktlig måte.

## Se også:

- [Rust docs for println!](https://doc.rust-lang.org/std/macro.println.html)
- [Rust docs for fmt modulen](https://doc.rust-lang.org/std/fmt/index.html)
- [The Rust Programming Language - Debugging Techniques](https://doc.rust-lang.org/book/ch14-00-more-about-cargo.html#debugging-techniques)