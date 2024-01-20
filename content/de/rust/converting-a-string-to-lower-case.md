---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Das Umwandeln von Strings in Kleinbuchstaben in Rust

## Was & Warum?

Das Konvertieren eines Strings in Kleinbuchstaben ist eine Standardaktion in der Programmierung, bei der alle Großbuchstaben im String in Kleinbuchstaben umgewandelt werden. Diese Umwandlung ist nützlich, um Texteingaben zu standardisieren und Sensitive Case-Ausgaben zu vermeiden.

## Wie:

Hier ist ein kurzer und einfacher Code, der einen gegebenen String in Kleinbuchstaben konvertiert.

```Rust
let s = "Hallo Welt!";
let lowercase = s.to_lowercase();
println!("{}", lowercase);
```

Wenn Sie diesen Code ausführen, erhalten Sie „hallo welt!“ als Ausgabe, welches der Eingabestring in Kleinbuchstaben ist.

## Vertiefung

Das Konvertieren von Strings in Kleinbuchstaben ist eine allgemein anerkannte Praxis in der Programmierwelt und hat seine Wurzeln in der Frühzeit der digitalen Textverarbeitung. Da Computer buchstabenbasierte Eingaben nicht verstehen können, werden sie in den meisten Fällen als Groß- und Kleinbuchstaben interpretiert, so dass "A" anders als "a" behandelt wird.

Als Alternative zur Methode `.to_lowercase()` könnten Sie durch jede Zeichenkette iterieren und die Methode `.to_ascii_lowercase()` verwenden, die nur ASCII-Zeichen behandelt. Dies wäre jedoch ineffizienter und würde nicht mit nicht-ASCII-Zeichen umgehen können.

Die Implementierung dieser Funktion in Rust berücksichtigt den Unicode-Standard. Während andere Sprachen möglicherweise nur die ASCII-Tabelle verwenden, um diese Konvertierung durchzuführen, erkennt Rust Unicode-Zeichen und stellt sicher, dass die Funktion korrekt für diese Zeichen funktioniert.

## Siehe auch

Um mehr über die Verarbeitung von Strings in Rust zu lernen, konsultieren Sie die offizielle Rust-Dokumentation und besonders den Abschnitt `str`. Hier sind einige Links, die Ihnen helfen werden:

- [Official Rust Documentation](https://doc.rust-lang.org/std/)
- [Rust `str` Type Documentation](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust by Example: Strings](https://doc.rust-lang.org/rust-by-example/std/str.html) 

Ein Verständnis der Textverarbeitung ist entscheidend für die Behandlung von Benutzereingaben, also stellen Sie sicher, dass Sie mit diesen Konzepten vertraut sind!