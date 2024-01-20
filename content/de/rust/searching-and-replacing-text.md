---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen von Text ist ein grundlegender Prozess, bei dem ein bestimmter Textabschnitt lokalisiert und dann durch einen anderen Text ersetzt wird. Programmierer nutzen dies häufig zur Textmanipulation oder zur automatischen Anpassung von Code.

## So geht's:
Um in Rust Text zu suchen und zu ersetzen, verwenden wir die `replace()`-Methode der `String`-Klasse. Hier ist ein einfaches Beispiel dazu:

```Rust
fn main() {
    let mut text = String::from("Hallo Welt!");
    text = text.replace("Welt", "Rust");
    println!("{}", text);
}
```

Die Ausgabe des Programms würde sein:

```Rust
Hallo Rust!
```

In diesem Beispiel ersetzten wir "Welt" durch "Rust".

## Deep Dive
Das Konzept des Suchens und Ersetzens von Text begann mit den ersten Texteditoren und ist seinen Ursprungszeiten immer noch sehr ähnlich. In Rust verwenden wir die eingebaute `replace()`-Funktion, aber es gibt auch andere Methoden und Bibliotheken wie `regex` für komplexere Anforderungen.

Während `replace()` eine direkte Textübereinstimmung verwendet, können Sie mit `regex::Regex::new()` und `regex::Regex::replace_all()` eine leistungsfähigere, reguläre Ausdrucksbasierte Such- und Ersetzfunktion ausführen.

Die Implementierung der `replace()`-Methode in Rust ist einfach und effizient. Sie durchsucht den `String` Zeichen für Zeichen und erstellt dabei einen neuen `String`, wenn eine Übereinstimmung gefunden wird.

## Siehe Auch
Für weiterführende Informationen empfehle ich folgende Links:
- Rust String Dokumentation: https://doc.rust-lang.org/std/string/struct.String.html
- Rust Regex Dokumentation: https://docs.rs/regex/1.3.9/regex/