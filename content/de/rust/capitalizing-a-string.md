---
title:                "Einen String großschreiben"
html_title:           "Rust: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Grossschreiben einer Zeichenkette ist die Umwandlung des ersten Buchstabens jedes Wortes in Großbuchstaben. Programmierer machen dies meistens für die Formatierung der Benutzerausgabe oder um Sortier- und Suchoperationen zu standardisieren.

## Wie macht man das:

In Rust erreichen Sie dies, indem Sie die Methode `to_uppercase()` in Kombination mit dem Iterator `chars()`. Hier ein Beispiel:

```Rust
fn haupt() {
    Lass uns eingabe = "hallo welt";
    Lass uns ergebnis: String = eingabe.chars()
        .enumerate()
        .map(|(i, c)| if i == 0 || eingabe.chars().nth(i - 1).unwrap() == ' ' { c.to_uppercase().collect::<String>() } else { c.to_lowercase().collect::<String>() })
        .collect();
    println!("{}", ergebnis);
}
```

Wenn Sie dieses Programm ausführen, erhalten Sie: `Hallo Welt`

## Vertiefung:

Das Konzept des Großschreibens von Zeichenketten stammt aus der Zeit der gedruckten Medien, wo es zur Hervorhebung und besseren Lesbarkeit genutzt wurde. In Rust können Sie die Eingebaute Methode `to_uppercase()` verwenden, es gibt jedoch auch Alternativen wie die Bibliothek `unicase` für komplexere Anforderungen.

Die Methode `to_uppercase()` in Rust arbeitet teichenweise, was bedeutet, dass sie jedes Zeichen in einen Unicode-Skalarwert umwandelt, ihn in Großbuchstaben umwandelt und anschließend in einen String zurückführt. 

## Weitere Informationen:

- [Rust Buch: Mehr über Strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Rust Documentation: Methode to_uppercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Bibliothek 'unicase'](https://docs.rs/unicase/2.6.0/unicase/)