---
title:                "Rust: Die Länge eines Strings finden"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

In diesem Blogpost geht es um die Länge von Strings und wie sie in Rust programmiert werden kann. Das Wissen darüber ist unerlässlich für alle, die mit Strings in ihren Programmen arbeiten. Also lass uns loslegen und herausfinden, wie wir die Länge eines Strings in Rust finden können!

## Wie geht das?

Um die Länge eines Strings in Rust zu finden, können wir die integrierte Funktion `len()` verwenden. Diese Funktion gibt die Anzahl der Bytes des Strings zurück. Hier ist ein einfaches Beispiel:

```Rust
let string = "Hallo Welt";
let length = string.len();
println!("{}", length);
```

Die Ausgabe wäre `10`, da der String "Hallo Welt" aus 10 Bytes besteht.

Es ist wichtig zu beachten, dass die `len()` Funktion die Anzahl der Bytes und nicht der Buchstaben zählt. Da Rust UTF-8 als Standard-Encoding verwendet, können Buchstaben mit mehr als einem Byte belegt sein.

Um die Anzahl der Buchstaben im String zu zählen, können wir die `chars()` Methode verwenden und dann die `count()` Funktion anwenden. Hier ein Beispiel:

```Rust
let string = "Hello World";
let char_count = string.chars().count();
println!("{}", char_count);
```

Die Ausgabe wäre wiederum `11`, da der String aus 11 Buchstaben besteht.

## Tiefer eintauchen

Es ist auch möglich, die Anzahl der Bytes eines bestimmten Zeichens in einem String zu finden. Dafür können wir die `char_indices()` Methode verwenden, die eine Iterator-Schnittstelle zurückgibt. Hier ein Beispiel:

```Rust
let string = "Hello 🌎";
for (i, c) in string.char_indices() {
    println!("Index: {}, Zeichen: {}", i, c);
}

```

Die Ausgabe sieht folgendermaßen aus:

```
Index: 0, Zeichen: H
Index: 1, Zeichen: e
Index: 2, Zeichen: l
Index: 3, Zeichen: l
Index: 4, Zeichen: o
Index: 5, Zeichen:  
Index: 6, Zeichen: 🌎
```

Wie du sehen kannst, gibt es für das Emoji 🌎 mehrere Byte-Indizes. Dies liegt daran, dass es aus zwei Bytes besteht.

## Siehe auch

- Die offizielle Rust Dokumentation zur `len()` Funktion: https://doc.rust-lang.org/std/primitive.str.html#method.len
- Ein Artikel über UTF-8 und Strings in Rust: https://wiki.sei.cmu.edu/confluence/display/RUST/STR+36%3A+Ensure+that+the+length+of+a+string+is+what+is+expected
- Die offizielle Rust Dokumentation zur `chars()` Methode: https://doc.rust-lang.org/std/string/struct.String.html#method.chars