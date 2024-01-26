---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Großschreibung wandelt alle Buchstaben eines Strings in Großbuchstaben um. Das wird gemacht, um Text zu betonen, Überschriften hervorzuheben oder einheitliche Datenformate zu gewährleisten.

## So geht’s:
```Rust
fn main() {
    let text = "rust ist toll!";
    let capitalized = text.to_uppercase();

    println!("{}", capitalized); // RUST IST TOLL!
}
```
Ausgabe:
```
RUST IST TOLL!
```

## Tiefgang:
Die `.to_uppercase()`-Methode in Rust ist relativ neu, verglichen mit anderen Sprachen. Sie nutzt das Unicode-Standardverhalten für Großschreibung und geht dabei über einfache ASCII-Zeichen hinaus. Alternativen wie `.to_ascii_uppercase()` existieren, funktionieren jedoch nur für ASCII und sind limitiert. 

Das Kapitalisieren ist keine billige Operation, da sie über alle Grapheme des Strings iterieren und kulturell korrekte Transformationen anwenden muss. Speziell für Rust ist die Situation, dass der Eigentümer des Strings eine kopierte, transformierte Version erhält, was bedeutet, dass die Originaldaten unverändert bleiben und die Kapitalisierung zusätzlichen Speicher benötigt. 

## Siehe auch:
- Rust Dokumentation zu `str::to_uppercase`: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase
- Unicode-Standards für Großbuchstaben: https://www.unicode.org/reports/tr21/tr21-5.html
- Ein Blogbeitrag über Fallumwandlungen in Rust: https://www.rust-lang.org/en-US/blog.html
