---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:31.701608-07:00
description: 'Hoe te: Soms heb je een string met gemengde aanhalingstekens, zoals
  dit.'
lastmod: '2024-04-05T21:53:50.603575-06:00'
model: gpt-4-0125-preview
summary: Soms heb je een string met gemengde aanhalingstekens, zoals dit.
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe te:
```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hallo, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // Uitvoer: Hallo, Rustaceans!
}
```

Soms heb je een string met gemengde aanhalingstekens, zoals dit:

```Rust
fn main() {
    let mixed_quoted = "'Rust zegt: \"Hallo, Wereld!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // Uitvoer: Rust zegt: "Hallo, Wereld!"
}
```

Hier worden alleen de buitenste enkele aanhalingstekens verwijderd.

## Diepere Duik
Als je aanhalingstekens uit een string verwijdert, vraag je je misschien af waarom het niet gewoon een simpele `.replace("\"", "")` is. In het begin was omgaan met tekst minder gestandaardiseerd, en hadden verschillende systemen verschillende manieren om tekst op te slaan en over te dragen, vaak met een soort 'escape sequentie' voor speciale tekens. Rust's `trim_matches` methode is veelzijdiger, hiermee kun je meerdere tekens specificeren om te trimmen, en of je vanaf het begin (prefix), het einde (suffix), of beide kanten van de string wilt trimmen.

Er zijn natuurlijk alternatieven. Regex is de krachtpatser voor stringmanipulatie, in staat om complexe patronen te matchen, en zou overkill zijn voor alleen het verwijderen van aanhalingstekens. Bibliotheken zoals `trim_in_place` kunnen in-place trimming bieden zonder de overhead van het creëren van een nieuw `String` object, wat wenselijk kan zijn voor prestatiekritische toepassingen.

Onder de motorkap itereert `trim_matches` daadwerkelijk door de karakters van de string vanaf beide uiteinden, controlerend tegen het opgegeven patroon tot een niet-overeenkomend karakter wordt gevonden. Het is efficiënt voor wat het doet, maar wees je er altijd van bewust dat het werkt met Unicode scalarwaarden. Als je string mogelijk multi-byte Unicode-karakters bevat, hoef je je geen zorgen te maken dat het ze opbreekt.

## Zie Ook
- Rust's documentatie over stringmanipulatie: https://doc.rust-lang.org/book/ch08-02-strings.html
- De `regex` crate voor complexe patronen: https://crates.io/crates/regex
- Rust aan de Hand van Voorbeelden voor praktische codeerscenario's: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
