---
title:                "Rust: Zeichenketten verknüpfen"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele verschiedene Gründe, warum man in der Programmierung Strings miteinander verbinden möchte. Das kann zum Beispiel dazu dienen, längere Textausgaben zu erstellen oder dynamisch generierte Inhalte zusammenzufügen.

## Wie das geht
In Rust gibt es verschiedene Möglichkeiten, um Strings zu verbinden. Die einfachste Variante ist die Verwendung des `+` Operators. Hier ein Beispiel:

```Rust
let firstname = "Max";
let lastname = "Mustermann";
let full_name = firstname + " " + lastname;

println!("Mein Name ist {}", full_name);
```
Dieser Code würde folgende Ausgabe erzeugen: `Mein Name ist Max Mustermann`.

Es gibt jedoch auch noch andere Methoden, um Strings zusammenzufügen. Eine davon ist die Verwendung der `format!` Macro. Hier ein Beispiel:

```Rust
let name = "Lena";
let greeting = format!("Hallo, {}!", name);

println!("{}", greeting);
```
Dieser Code würde folgende Ausgabe erzeugen: `Hallo, Lena!`.

## Tiefergehende Informationen
Beim Verbinden von Strings gibt es einige Dinge zu beachten. Zum Beispiel kann es bei der Verwendung des `+` Operators zu Performance-Problemen kommen, da er jedes Mal einen neuen String alloziert. Es kann daher manchmal effizienter sein, die `format!` Macro zu verwenden.

Außerdem ist es wichtig, auf die Typisierung der Strings zu achten. Rust unterscheidet zwischen `&str` und `String`, wobei `&str` für statische und `String` für dynamische Strings verwendet werden. Beim Verbinden von Strings müssen diese beiden Typen entsprechend beachtet werden.

## Siehe auch
- https://doc.rust-lang.org/std/string/struct.String.html
- https://doc.rust-lang.org/std/string/struct.String.html#method.push_str
- https://doc.rust-lang.org/std/macro.format.html