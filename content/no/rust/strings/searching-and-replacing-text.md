---
title:                "Søking og erstatting av tekst"
aliases:
- /no/rust/searching-and-replacing-text.md
date:                  2024-01-20T17:58:52.106224-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å søke og erstatte tekst lar oss finne strenger i data og bytte dem ut med nye strenger. Programmererne trenger dette for å oppdatere data, korrigere feil, eller endre formatet på informasjon.

## Hvordan:
```Rust
fn main() {
    let tekst = "Hei på deg, verden!";
    let ny_tekst = tekst.replace("verden", "Norge");
    println!("{}", ny_tekst);
}
```
Output:
```
Hei på deg, Norge!
```

## Dypdykk
Søk og erstatt har røtter i tekstredigering fra 60-tallet. Det har utviklet seg fra enkle kommandolinjeverktøy til komplekse funksjoner i programmeringsspråk. I Rust, bruker vi `.replace()`-metoden for String-objekter, som er både effektiv og enkel. Denne metoden fungerer bra for enkle bytter, men for mer komplekse mønstre kan man bruke regex-biblioteket. Biblioteket tillater søk med avanserte mønstre (regular expressions) og gir fleksible erstatningsfunksjoner. Til tross for ytelsen til `.replace()`, er regex mer kraftfullt for mønstersøk, men det kan være tregere og mer ressurskrevende.

## Se Også
- [The Rust Programming Language Book – Text Processing](https://doc.rust-lang.org/book/ch08-02-strings.html#concatenation-with-the--operator-or-the-format-macro)
- [Rust by Example – String replace](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust Regex Crate Documentation](https://docs.rs/regex/latest/regex/)
