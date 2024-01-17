---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Rust: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine gängige Aufgabe in der Programmierung. Oftmals werden diese Zeichen entfernt, um Daten zu bereinigen oder zu transformieren, bevor sie weiterverarbeitet werden. Programmer greifen immer wieder auf dieses Verfahren zurück, um ihre Programme effizienter und robuster zu gestalten.

# Wie geht's?

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in Rust auf verschiedene Arten erreicht werden. Die einfachste Methode ist die Verwendung der `replace()` Funktion, die durch die Angabe des zu ersetzenden Musters und des entsprechenden Ersatzes die Zeichen im Text ersetzt. Ein Beispiel für die Anwendung dieser Funktion sieht wie folgt aus:

```Rust
let mut text = String::from("Hallo Welt!");
text.replace("!", "");
println!("{}", text);
```

Die Ausgabe dieses Codeschnipsels würde "Hallo Welt" ohne das Ausrufezeichen sein. Alternativ kann auch die `filter()` Methode verwendet werden, die ein vorgegebenes Prädikat auf den Text anwendet und alle Zeichen entfernt, die dem Muster entsprechen. Hier ein Beispiel hierfür:

```Rust
let text = String::from("Hallo Welt!");
let filtered_text = text.chars().filter(|c| c != &'!').collect::<String>();
println!("{}", filtered_text);
```

Dieser Ansatz ist etwas komplexer, erlaubt aber auch die Anwendung von komplexeren Bedingungen.

# Tiefes Eintauchen

Das Entfernen von Zeichen mit einer bestimmten Form ist schon lange eine gängige Aufgabe in der Programmierung. In den Anfängen der Computerentwicklung war das Löschen von Zeichen oft ein komplexer und zeitaufwändiger Prozess. Mit dem Fortschritt in der Technologie wurden jedoch immer effizientere Methoden entwickelt, die es Programmern ermöglichen, diese Aufgabe schnell und einfach zu erledigen. Alternativen zum Löschen von Zeichen können auch das Ersetzen oder Filtern beinhalten.

In Rust wird die `replace()` Funktion von der `String` Datenstruktur zur Verfügung gestellt, während `filter()` eine Methode der `Chars` Datenstruktur ist. Beide Methoden verwenden die von Rust bereitgestellten String-Manipulations-Funktionen, um das Entfernen von Zeichen auf eine effiziente und elegante Weise zu ermöglichen.

# Siehe auch

- [How to Replace Substrings in Rust](https://www.baeldung.com/rust/replace-substrings)
- [Rust Documentation on Strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Filtering and Mapping With Closures in Rust](https://gavialonsoleon.com/filtering-and-mapping-with-closures-in-rust/)