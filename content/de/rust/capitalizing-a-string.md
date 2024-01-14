---
title:                "Rust: String großschreiben"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man string capitalization in Rust verwenden? Die Antwort liegt in der Notwendigkeit, eine klare und konsistente Darstellung von Zeichenketten zu gewährleisten. Indem man die ersten Buchstaben von Wörtern groß schreibt, können die Lesbarkeit und die Struktur von Texten verbessert werden.

## Wie funktioniert es?

Um eine Zeichenkette in Rust zu capitalisieren, muss man die Methode `.to_uppercase()`anwenden. Hier ist ein Beispielcode, der eine Zeichenkette in Großbuchstaben umwandelt:

```Rust
let s = "hallo";
let s_capitalized = s.to_uppercase();
Println!("Kapitalisierte Zeichenkette: {}", s_capitalized);
```

Die Ausgabe wird wie folgt sein:

```
Kapitalisierte Zeichenkette: HALLO
```

## Tiefere Einblicke

Unter der Haube verwendet Rust das `Unicode Character Database (UCD)`, um zu bestimmen, welche Buchstaben in einer bestimmten Sprache groß geschrieben werden sollen. Das bedeutet, dass die Methode `.to_uppercase()` nicht nur für englische Texte funktioniert, sondern auch für andere Sprachen wie Deutsch, Französisch oder Spanisch.

Zusätzlich ermöglicht Rust auch die Verwendung von `Unicode Normalization`, um eine Zeichenkette in eine bestimmte Form zu bringen, bevor sie capitalisiert wird. Das ist hilfreich, um sicherzustellen, dass die Zeichenkette konsistent und einheitlich formatiert ist.

## Siehe auch

- [Rust String Dokumentation](https://doc.rust-lang.org/std/string/)
- [UCD Offizielle Webseite](https://unicode.org/ucd/)
- [Unicode Normalization in Rust](https://doc.rust-lang.org/std/string/struct.String.html#method.nfc)