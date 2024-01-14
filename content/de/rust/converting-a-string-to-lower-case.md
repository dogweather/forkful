---
title:                "Rust: Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

"## Warum"

Rust ist eine Programmiersprache, die für die Entwicklung von sicheren und performanten Anwendungen bekannt ist. Eine der grundlegenden Aufgaben in der Programmierung ist die Bearbeitung von Texten, und dies gilt auch für Rust. In diesem Blog-Beitrag geht es darum, wie man eine Zeichenfolge in Kleinbuchstaben umwandelt.

## Wie geht man vor?

Die Rust Standardbibliothek bietet eine Methode, um eine Zeichenfolge in Kleinbuchstaben umzuwandeln: die Methode `to_lowercase()`. Hier ist ein Beispielcode, wie man diese Methode verwendet:

```Rust
let s = String::from("HELLO RUST PROGRAMMERS");

let lower_case = s.to_lowercase();

println!("{}", lower_case); // output: hello rust programmers
```

Die Methode `to_lowercase()` gibt eine neue Zeichenfolge zurück, die alle Buchstaben in Kleinbuchstaben enthält. Um diese Methode verwenden zu können, muss die Zeichenfolge den Typ `String` haben, da die Methode darauf definiert ist.

Bei der Benutzung von `to_lowercase()` muss man beachten, dass die Methode keine Unicode-Normalisierung durchführt. Dies bedeutet, dass Zeichen wie "ß" oder "ä" nicht in ihre entsprechenden Einzelzeichen umgewandelt werden.

## Tiefergehende Einblicke

Beim Konvertieren einer Zeichenfolge in Kleinbuchstaben sollten Entwickler auch die Leistung im Auge behalten. Um die Bearbeitungsgeschwindigkeit zu verbessern, sollte man es vermeiden, die Zeichenfolge mehrmals durchlaufen zu lassen. Stattdessen kann man die Methode `to_lowercase()` in einer Kette von Methodenaufrufen verwenden, um die Zeichenfolge in Kleinbuchstaben umzuwandeln und gleichzeitig andere Bearbeitungen durchzuführen.

Zusätzlich gibt es auch die Methode `make_ascii_lowercase()`, die eine Zeichenfolge in ASCII-Zeichen in Kleinbuchstaben umwandelt. Diese Methode kann nützlich sein, wenn man sicherstellen möchte, dass eine Zeichenfolge nur aus ASCII-Zeichen besteht.

## Siehe auch

- Rust Standardbibliothek Dokumentation: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Mehr über Rust Programmierung: https://www.rust-lang.org/learn
- Unicode-Normalisierung: https://unicode.org/reports/tr15/