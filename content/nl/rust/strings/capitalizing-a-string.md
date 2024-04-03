---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:55.953893-07:00
description: "Hoe: Rust bevat geen ingebouwde methode om elk woord in een string te\
  \ kapitaliseren, maar we kunnen eenvoudig onze eigen methode implementeren met behulp\u2026"
lastmod: '2024-03-13T22:44:50.576787-06:00'
model: gpt-4-0125-preview
summary: Rust bevat geen ingebouwde methode om elk woord in een string te kapitaliseren,
  maar we kunnen eenvoudig onze eigen methode implementeren met behulp van de `to_ascii_uppercase`
  methode voor enkele karakters en door de woorden te doorlopen.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe:
Rust bevat geen ingebouwde methode om elk woord in een string te kapitaliseren, maar we kunnen eenvoudig onze eigen methode implementeren met behulp van de `to_ascii_uppercase` methode voor enkele karakters en door de woorden te doorlopen.

```Rust
fn capitalize_words(s: &str) -> String {
    s.split_whitespace()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

fn main() {
    let zin = "hallo wereld";
    println!("{}", capitalize_words(zin));
}
```

Voorbeelduitvoer:

```
Hallo Wereld
```

## Diepgaand:
Historisch gezien heeft Rust prioriteit gegeven aan een minimale standaardbibliotheek, met veel nutsfuncties die door de gemeenschap worden aangeboden via crates. Voor stringkapitalisatie kun je de `heck` crate gebruiken voor meer geavanceerde gevalconversies, zoals CamelCase, snake_case en meer.

Het kapitaliseren van een string kan lastig zijn met Unicode-karakters. Het `char`-type van Rust is een Unicode scalar-waarde, wat zorgt voor de juiste afhandeling van de meeste karakters. Bij het omgaan met volledige Unicode-normalisatie moeten geavanceerdere bibliotheken, zoals `unicode-segmentation`, worden overwogen voor operaties die rekening houden met grafeemclusters.

Qua implementatie is onze `capitalize_words` functie niet erg prestatiegericht omdat het een nieuwe `String` voor elk woord toewijst. In toepassingen die hoge prestaties vereisen, zou het voordelig zijn om stringmanipulatie te optimaliseren om overmatige geheugentoewijzingen te vermijden.

## Zie Ook:
- Rust-documentatie voor 'char': https://doc.rust-lang.org/std/primitive.char.html
- 'Heck' crate voor gevalconversies: https://crates.io/crates/heck
- 'Unicode Normalization Forms' in Rust: https://unicode-rs.github.io/unicode-normalization/unicode_normalization/index.html
- Rust Boek voor meer over strings: https://doc.rust-lang.org/book/ch08-02-strings.html
