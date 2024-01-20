---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilstrings ist der Prozess des Entnehmens spezifischer kleinerer Sätze aus einer größeren Zeichenkette. Programmierer machen das, um Daten zu filtern oder zu untersuchen.

## Wie macht man das:

In Rust, benutzen wir die `slice` Methode, um Teilstrings zu gewinnen. 

```Rust
fn main() {
    let s = "Hallo, Welt";
    let teil = &s[0..5];
    println!("{}", teil);
}
```

Dieser Code gibt "Hallo" aus, der erste Teilstring in unserem Originalstring.

## Tiefere Tauchgang

Die `slice` Methode in Rust beruht auf den Funktionen zur Zeichenkettenverarbeitung aus C, der Mutter von Rust. Aber anders als C, Rust Priorität auf die Sicherheit und verhindert so Zugriffe außerhalb des Teilstrings.

Ein populärer Ansatz, um automatisch Teilstrings in Rust zu finden, ist die Verwendung von regulären Ausdrücken mit der `regex` Bibliothek. 

Die Implementation von Rusts Teilstringextrahierung ist einfach, wenn der Teilstring innerhalb einer einzelnen Zeichenkette liegt. Bei komplizierteren Strukturen, wie verschachtelten Schleifen, können die Dinge schnell unübersichtlich werden.

## Siehe Auch

1. [Offizielle Rust Dokumentation zur `Slice` Methode](https://doc.rust-lang.org/std/string/struct.String.html#method.slice)
2. [Reguläre Ausdrücke in Rust](https://docs.rs/regex/1.3.9/regex/)
3. [Rusts Sicherheitsgarantien und wie sie Teilstrings beeinflussen](https://doc.rust-lang.org/book/ch04-03-slices.html)