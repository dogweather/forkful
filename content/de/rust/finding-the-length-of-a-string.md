---
title:                "Die Länge eines Strings bestimmen"
html_title:           "Rust: Die Länge eines Strings bestimmen"
simple_title:         "Die Länge eines Strings bestimmen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge eines Strings zu finden ist eine grundlegende Aufgabe in der Programmierung, die oft bei der Verarbeitung von Texten benötigt wird. Es ist wichtig zu verstehen, wie dies in Rust funktioniert, um effektive und effiziente Code zu schreiben.

## Wie geht's

Die Länge eines Strings in Rust kann mit der `len()` Methode gefunden werden. Die folgende Codezeile zeigt, wie eine Variable `text` erstellt und seine Länge mit `len()` gefunden werden kann:

```Rust
let text = "Hallo Welt!";
println!("Die Länge des Strings ist: {}", text.len());
```

Der Output des obigen Codes wird sein:

`Die Länge des Strings ist: 11`

Die `len()` Methode gibt die Anzahl der UTF-8 kodierten Bytes zurück, die im String enthalten sind. Dies bedeutet, dass auch Sonderzeichen wie Umlaute oder Emoji korrekt gezählt werden.

## Tief tauchen

In Rust wird die Länge eines Strings durch die Anzahl der Bytes gemessen, nicht durch die Anzahl der Zeichen. Dies kann zu unerwarteten Ergebnissen führen, wenn man mit mehrsprachigen Texten arbeitet, da manche Zeichen mehr als ein Byte belegen. Um die tatsächliche Anzahl der Zeichen zu erhalten, kann die `chars()` Methode verwendet werden, die einen Iterator über die Unicode-Codepoints im String zurückgibt. Die Länge kann dann mit dem `count()` Operator gefunden werden, wie in diesem Beispiel gezeigt:

```Rust
let text = "こんにちは 世界!";
println!("Die Länge des Strings ist: {}", text.chars().count());
```

Der Output dieses Codes wird sein:

`Die Länge des Strings ist: 8`

Wenn man die Anzahl der Zeichen in einem String berechnen muss, ist es daher ratsam, die `chars()` Methode zu verwenden, um genaue Ergebnisse zu erzielen.

## Siehe auch

- [Rust-Dokumentation zu Strings](https://doc.rust-lang.org/std/string/index.html)
- [Tutorial über Strings in Rust](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Weitere Informationen über die `len()` Methode](https://doc.rust-lang.org/std/primitive.str.html#method.len)