---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en tekststreng betyr å gjøre første bokstav i hvert ord til en stor bokstav. Programmere gjør dette for å standardisere formatering, ofte for visning av titler eller overskrifter.

## Slik gjør du:
Rusts standardbibliotek tilbyr ingen innebygd metode for å kapitalisere hver ord i en streng, men vi kan bruke `to_uppercase()` sammen med egendefinert logikk. Her er et eksempel:

```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first_char) => first_char.to_uppercase().collect::<String>() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let greeting = "hei på deg, verden!";
    let capitalized = capitalize_words(greeting);
    println!("{}", capitalized); // "Hei På Deg, Verden!"
}
```

## Dykk dypere
Historisk sett har kapitalkonvertering av tekst vært standard praksis i typografi, brukt til å fremheve titler og navn. I mange programmeringsspråk er operasjonen tilgjengelig som en enkel funksjon. Rust, derimot, fokuserer på sikkerhet og ytelse fremfor å inkludere mange hjelpefunksjoner.

Alternativer for kapitalisering inkluderer biblioteker som `regex` for komplekse mønstergjenkjenninger, eller `unicode-segmentation` for riktig behandling av Unicode tekst.

Når du implementerer en slik funksjon, er det viktig å håndtere Unicode og flerbyte-karakterer korrekt. Rust bruker UTF-8-strenger, så operasjonen håndteres på et skalerbart og internasjonalt nivå.

## Se også
- Rust Docs på `to_uppercase()`: https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase
- Regex Crate: https://crates.io/crates/regex
- Unicode Segmentation Crate: https://crates.io/crates/unicode-segmentation