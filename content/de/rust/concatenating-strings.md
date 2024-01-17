---
title:                "Verknüpfung von Zeichenketten"
html_title:           "Rust: Verknüpfung von Zeichenketten"
simple_title:         "Verknüpfung von Zeichenketten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Concatenating Strings ist der Prozess des Zusammenfügens von zwei oder mehr Strings in einen einzelnen String. Programmierer nutzen diese Methode, um eine längere und vollständigere Ausgabe zu erhalten, die aus mehreren Bestandteilen besteht.

## Wie geht das?
Um Strings in Rust zu concatenaten, können wir den `+` Operator verwenden oder die `format!` Macro nutzen. Hier ist ein Beispiel:

```Rust
let hello = "Hallo";
let world = "Welt";

// Verwendung des + Operators
let greeting = hello + " " + world;

// Verwendung der format! Macro
let greeting = format!("{} {}", hello, world);
```

Dies würde den String "Hallo Welt" ergeben und zu einer vollständigen Begrüßung kombinieren.

## Tiefere Einblicke
Das Konzept des Zusammenfügens von Strings findet sich in vielen Programmiersprachen und hat seine Wurzeln in der Algorithmenanalyse, wo es als "Stringverkettung" bezeichnet wird. Alternativ können auch andere Methoden wie das Verwenden von Arrays oder die Verwendung von Zeichenkettenmanipulationsfunktionen wie `push_str` in Rust verwendet werden. Um effiziente Stringconcatenation in Rust zu erreichen, verwendet die Standardbibliothek ein "Builder-Muster", das den Prozess der Verkettung optimiert.

## Siehe auch
Offizielle Rust-Dokumentation für Strings: https://doc.rust-lang.org/std/string/index.html
String-Building in anderen Programmiersprachen: https://stackify.com/string-concatenation-performance-python-java-cpp/