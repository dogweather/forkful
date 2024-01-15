---
title:                "String in Kleinbuchstaben umwandeln"
html_title:           "Rust: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal mit Texten in deinen Programmierprojekten gearbeitet hast, wirst du wahrscheinlich schon das Bedürfnis gehabt haben, alle Zeichen in einem String in Kleinbuchstaben zu konvertieren. Das kann aus verschiedenen Gründen nützlich sein, zum Beispiel um die Eingabe von Benutzern zu standardisieren oder um Texte für Vergleiche zu normalisieren.

## Wie geht man vor?

Um einen String in Rust in Kleinbuchstaben zu konvertieren, gibt es verschiedene Möglichkeiten. Die einfachste ist die Verwendung der Funktion `to_lowercase()` aus der Standard-Bibliothek. Hier ist ein Beispielcode, der diese Funktion verwendet:

```Rust
let text = "HALLO WELT";
let converted_text = text.to_lowercase();
println!("{}", converted_text); // gibt "hallo welt" aus
```

Man kann auch die Methode `to_lowercase()` direkt auf einen String aufrufen, ohne die Funktion zu verwenden:

```Rust
let text = "HALLO WELT";
let converted_text = text.to_lowercase();
println!("{}", converted_text); // gibt "hallo welt" aus
```

Eine weitere Möglichkeit ist die Verwendung der Methode `chars()` und der Funktion `collect()` aus der Standard-Bibliothek, um jedes Zeichen in einen Vektor zu konvertieren und dann mit Hilfe der `to_lowercase()`-Methode jedes Zeichen in Kleinbuchstaben umzuwandeln. Hier ist ein Beispiel:

```Rust
let text = "HALLO WELT";
let mut chars: Vec<char> = text.chars().collect();
for c in &mut chars {
    *c = c.to_lowercase().next().unwrap();
}
let converted_text: String = chars.into_iter().collect();
println!("{}", converted_text); // gibt "hallo welt" aus
```

## Tiefere Einblicke

Bei der Konvertierung von Strings in Kleinbuchstaben sind einige Dinge zu beachten. Zum einen kann es je nach Sprache und Schreibweise Unterschiede bei der Umwandlung von Groß- zu Kleinbuchstaben geben. In manchen Fällen können sogar mehrere Kleinbuchstaben aus einem Großbuchstaben entstehen.

Wie in den Beispielen gezeigt, gibt es in Rust verschiedene Möglichkeiten, um Strings in Kleinbuchstaben zu konvertieren. Es ist jedoch wichtig zu beachten, dass diese Methoden nicht in allen Fällen das gleiche Resultat liefern. Es kann vorkommen, dass bestimmte Sonderzeichen oder diakritische Zeichen bei einer Methode anders behandelt werden als bei einer anderen.

## Siehe auch

- [Rust-Dokumentation: `to_lowercase()` Funktion](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Rust-Dokumentation: `chars()` Methode](https://doc.rust-lang.org/std/string/struct.String.html#method.chars)
- [Rust-Dokumentation: `collect()` Funktion](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect)