---
title:                "Rust: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Wenn du dich für das Lernen von Rust entschieden hast, wirst du festgestellt haben, dass es eine mächtige und wachsende Sprache mit einer starken Typisierung ist. Eines der häufigen Aufgaben beim Programmieren ist das Konvertieren von Strings in Kleinbuchstaben. Aber warum sollte man das überhaupt tun? Im Folgenden werden wir uns genauer mit diesem Thema befassen.

## So geht's
Um einen String in Rust in Kleinbuchstaben zu konvertieren, gibt es einen einfachen Weg mit der Funktion `to_lowercase()`. Hier ist ein Beispielcode, der einen String in Kleinbuchstaben umwandelt:

```Rust
let string = "Hallo, Rust!";
let string_lower = string.to_lowercase();
println!("{}", string_lower);
```
Die Ausgabe dieses Codes wird `hallo, rust!` sein.

Es ist auch möglich, Strings in Unicode zu konvertieren, indem man die Funktion `to_lowercase()` auf den Typ `Chars` anwendet. Hier ist ein Beispielcode:

```Rust
let string = "RUST 💻";
let mut char_iter = string.chars().peekable();
while let Some(char) = char_iter.next() {
    print!("{}", char.to_lowercase());
}
```
Die Ausgabe dieses Codes wird `rust 💻` sein.

## Tiefergehende Einblicke
Jetzt wo wir wissen, wie wir Strings in Kleinbuchstaben konvertieren, können wir einen tieferen Einblick in die Technik dahinter werfen. Die `to_lowercase()`-Funktion verwendet die Unicode-Standardisierung, um die Umwandlung korrekt durchzuführen. Es gibt jedoch mehrere Regeln und Ausnahmen bei der Umwandlung in Kleinbuchstaben, je nach Sprache und Alphabet. In Rust gibt es auch die Möglichkeit, spezifische Locale und Unicode-Methoden zu verwenden, um eine genauere Konvertierung durchzuführen.

## Siehe auch
- [Rust Dokumentation - Strings](https://doc.rust-lang.org/std/string/index.html)
- [Unicode-Standardisierung](https://unicode.org/standard/standard.html) 
- [Rust The Book - "Common Collections"](https://doc.rust-lang.org/book/ch08-03-hash-maps.html)