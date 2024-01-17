---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Rust: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sletting av karakterer som matcher et mønster er en vanlig oppgave for programmerere. Dette innebærer å fjerne bestemte tegn eller ord fra en tekststreng som følger et visst mønster. Dette kan være nyttig for å rense data eller filtrere ut uønskede elementer.

## Hvordan:
For å slette karakterer som matcher et mønster i Rust, kan du bruke standard bibliotekfunksjonen ```replace()```. Her er et eksempel på å fjerne alle vokaler fra en tekststreng:

```
let mut tekst = String::from("Hei, jeg heter Rust!");
tekst = tekst.replace(['a', 'e', 'i', 'o', 'u'].as_ref(), "");
println!("{}", tekst);
```

Dette vil resultere i utskriften "H, jg htr Rst!".

## Dypdykk:
Sletting av karakterer som matcher et mønster har vært en vanlig oppgave i programmering i lang tid. I Rust, har vi flere alternativer for å utføre denne oppgaven, som å bruke en regex-bibliotek eller et match uttrykk. Implementasjonen av ```replace()``` funksjonen i Rust er optimalisert for ytelse og har lavere kompleksitet enn noen av de andre alternativene.

## Se også:
- [Dokumentasjon for ```replace()``` funksjonen i Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
- [Rust regex bibliotek](https://docs.rs/regex/1.4.2/regex/)
- [Eksperimenter med Rust kode online](https://play.rust-lang.org/)