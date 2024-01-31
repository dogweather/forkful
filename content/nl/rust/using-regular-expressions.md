---
title:                "Reguliere expressies gebruiken"
date:                  2024-01-28T22:10:01.923448-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reguliere expressies gebruiken"

category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/rust/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Reguliere expressies, of kortweg regex, zijn reeksen van tekens die zoekpatronen vormen. Programmeurs gebruiken regex om tekst te zoeken, bewerken of te manipuleren door het matchen van complexe patronen, vaak voor validatie- of parsingdoeleinden.

## Hoe te:
Rust gebruikt de `regex` crate voor regex-operaties. Voeg het eerst toe aan je `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Vervolgens kun je strings matchen als volgt:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let datum = "2023-04-01";

    println!("Komt de tekst overeen met het datumpatroon? {}", re.is_match(datum));
}
```

Uitvoer:

```
Komt de tekst overeen met het datumpatroon? true
```

Voor het vastleggen van groepen:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.(\w+)").unwrap();
    let e-mail = "gebruiker@voorbeeld.com";

    match re.captures(email) {
        Some(caps) => {
            println!("Gebruiker: {}, Domein: {}, Extensie: {}", &caps[1], &caps[2], &caps[3]);
        }
        None => println!("Geen match gevonden."),
    }
}
```

Uitvoer:

```
Gebruiker: gebruiker, Domein: voorbeeld, Extensie: com
```

## Diepgaand
Regex bestaat al sinds de jaren 1950, met wortels in de automaattheorie en formele taal. Rust's `regex` module is gebouwd voor snelheid en veiligheid, met een focus op het compileren van efficiënte regex patronen tijdens runtime. Alternatieven voor regex omvatten stringfuncties zoals `find`, `split`, en `replace`, die eenvoudigere use-cases dekken zonder patronen. Regexen in Rust zijn bijzonder efficiënt vanwege uitgebreide optimalisatie en compilatie van de regex patronen.

## Zie Ook
- De `regex` crate documentatie: https://docs.rs/regex/
- Rust's boeksectie over regex: https://doc.rust-lang.org/book/ch18-00-patterns.html
- Hoofdstuk over Reguliere Expressies van "De Rust Programmeertaal": https://doc.rust-lang.org/stable/book/ch18-03-pattern-syntax.html
