---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Charaktere nach Muster Löschen in Rust

## Was & Warum

Musterbasiertes Löschen von Zeichen ist die Aktion, Zeichen aus einem Text zu entfernen, die einer bestimmten Regel oder einem Muster entsprechen. Programmierer machen dies, um Daten zu säubern oder Textformatierung zu handhaben.

## Wie zu:

Ein Weg, um Zeichen in Rust zu löschen, ist die Verwendung der Methode `replace()`. Sie erlaubt es uns, alle Instanzen eines Zeichen oder Muster durch nichts zu ersetzen, was effektiv dazu führt, dass es gelöscht wird.

```Rust
let s = "Hallo, Welt!";
let ohne_komma = s.replace(",", "");
println!("{}", ohne_komma); // Ausgabe: "Hallo Welt!"
```
In diesem Beispiel wurde das Komma gelöscht.

Wir können auch gängige Ausdrücke (regex) verwenden, um komplexere Muster zu matchen und zu löschen.

```Rust
use regex::Regex;
let re = Regex::new(r"\d").unwrap();
let s = "123abc456";
println!("{}", re.replace_all(&s, "")); // Ausgabe: "abc"
```
Hier werden alle Ziffern aus der Zeichenkette gelöscht.

## Deep Dive

Das Löschen von Zeichen nach einem Muster ist eine gebräuchliche Praxis in der Textverarbeitung und hat seinen Ursprung in frühen Programmiersprachen wie Perl und sed. In Rust gibt es mehrere Weg dies zu tun, abhängig von der Situation und der benötigten Kontrolle über den Vorgang.

Wir haben oben gesehen, wie wir `replace()` verwenden können, um einfach zu handhabende Fälle zu behandeln - einfach alle Instanzen eines Zeichen oder einer Zeichenkette durch nichts zu ersetzen.

Wir haben auch gesehen, wie gängige Ausdrücke verwendet werden können, um komplexere Muster zu matchen und zu löschen.

Wir sollten beachten, dass, obwohl gängige Ausdrücke leistungsfähig sind, ihre Verwendung manchmal zu Leistungseinbußen führen kann, da sie meist viel komplizierter sind als einfache Zeichen- oder Zeichenkettenvergleiche - also sollten sie vorsichtig verwendet werden.

## Siehe auch

- Rust Dokumentation: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- Paket `regex`: https://crates.io/crates/regex
- Rust Buch - Textverarbeitung: https://doc.rust-lang.org/book/ch08-03-hash-maps.html#storing-utf-8-encoded-text-with-strings