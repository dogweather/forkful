---
title:                "Unterstrings extrahieren"
html_title:           "Rust: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilzeichenketten, auch bekannt als Unterzeichenketten, ist eine gängige Praxis unter Programmierern. Dabei werden Teile einer längeren Zeichenkette ausgewählt und separat verwendet. Das kann hilfreich sein, um bestimmte Operationen auf nur einem Teil der Zeichenkette auszuführen oder einfach um den Code übersichtlicher zu gestalten.

## Wie geht's?

In Rust gibt es eine Vielzahl von Methoden, um Teilzeichenketten zu extrahieren. Ein Beispiel ist die Methode `slice`, die es uns ermöglicht, einen Teil einer Zeichenkette auszuwählen. Hier ein einfaches Beispiel:

```Rust
let string = "Hallo Welt";
let teil = &string[0..5];
```

Die Variable `teil` enthält nun den Wert "Hallo", da wir von Index 0 bis 5 (nicht inklusive) die Zeichenkette ausgewählt haben.

## Tiefergehend

Das Extrahieren von Teilzeichenketten ist eine Technik, die programmierübergreifend verwendet wird. Es kann besonders hilfreich sein, wenn man mit lange Zeichenketten arbeitet und nur einen Teil davon benötigt. In anderen Programmiersprachen gibt es auch ähnliche Methoden wie z.B. `substring` in Java oder `substr` in PHP.

In Rust erfolgt das Extrahieren von Teilzeichenketten ähnlich wie in anderen Sprachen. Es gibt jedoch einige feine Unterschiede, wie zum Beispiel die Verwendung von Slices statt Substrings. Es ist wichtig zu beachten, dass Slices in Rust immer eine Referenz auf den ursprünglichen Datentyp zurückgeben. Deshalb muss man bei der Verwendung von Slices darauf achten, dass die ursprüngliche Zeichenkette nicht gelöscht oder geändert wird, sonst ändert sich auch die Teilzeichenkette.

## Weitere Quellen

Für weitere Informationen kannst du die offizielle Rust-Dokumentation zum Thema Slice `https://doc.rust-lang.org/std/primitive.slice.html` oder den Rust-Playground `https://play.rust-lang.org/` nutzen, um selbst mit Teilzeichenketten zu experimentieren. Es gibt auch unzählige Ressourcen von der Community, wie z.B. Foren oder Tutorials, die bei der Anwendung von Teilzeichenketten in Rust helfen können.