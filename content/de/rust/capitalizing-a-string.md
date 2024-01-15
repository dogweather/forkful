---
title:                "Großschreibung einer Zeichenkette"
html_title:           "Rust: Großschreibung einer Zeichenkette"
simple_title:         "Großschreibung einer Zeichenkette"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte jemand sich dafür interessieren, einen String in Rust groß zu schreiben? Nun, es gibt mehrere Gründe dafür. Zum einen kann es sein, dass man bestimmte String-Manipulationen durchführen möchte, bei denen es wichtig ist, dass der String in der richtigen Formatierung ist. Zum anderen kann es auch für die Ästhetik des Codes wichtig sein, dass alle Strings einheitlich großgeschrieben sind.

## Wie es geht

Um einen String in Rust groß zu schreiben, gibt es mehrere Möglichkeiten. Eine Möglichkeit ist, die Funktion `to_uppercase()` auf dem String aufzurufen. Hier ein Beispiel:

```Rust
let string = String::from("hallo welt");
let uppercase_string = string.to_uppercase();
```
Dieser Code wandelt den String "hallo welt" in "HALLO WELT" um. Man kann auch eine bereits bestehende Variable nutzen, um den großgeschriebenen String zu speichern:

```Rust
let mut string = String::from("hallo welt");
string = string.to_uppercase();
```

Eine weitere Möglichkeit ist, das `String`-Modul aus der Standardbibliothek zu importieren und die Methode `make_ascii_uppercase()` auf dem String aufzurufen:

```Rust
use std::string::String;
let mut string = String::from("hallo welt");
string.make_ascii_uppercase();
```

Dies hat den gleichen Effekt wie `to_uppercase()`, jedoch wird hier der ASCII-Standard für die Großschreibung verwendet.

## Tiefere Einblicke

Beide Methoden, `to_uppercase()` und `make_ascii_uppercase()` nutzen intern die `chars()`-Methode auf dem String, um jede einzelne Zeichen des Strings zu überprüfen und gegebenenfalls in Großbuchstaben umzuwandeln. Dies kann nützlich sein zu wissen, wenn man eigene Funktionen für die Großschreibung von Strings schreiben möchte.

Zudem ist zu beachten, dass beide Methoden den ursprünglichen String nicht verändern, sondern stattdessen einen neuen String zurückgeben. Deshalb muss man den neuen String entweder einer neuen Variable zuweisen oder die ursprüngliche Variable überschreiben.

## Siehe auch
- [Rust Standardbibliothek String-Dokumentation](https://doc.rust-lang.org/std/string/struct.String.html)
- [How to Convert Strings to Upper or Lower Case in Rust](https://www.tutorialspoint.com/how-to-convert-strings-to-upper-or-lower-case-in-rust)
- [Rust By Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)