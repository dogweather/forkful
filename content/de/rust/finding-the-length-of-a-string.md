---
title:    "Rust: Die Länge eines Strings finden"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende Aufgabe in der Programmierung. Oft ist es notwendig, die Länge einer Zeichenkette zu kennen, um bestimmte Funktionen auszuführen oder die Ausgabe eines Programms zu formatieren. In dieser Blog-Post werden wir uns ansehen, wie man dies in der Programmiersprache Rust erreichen kann.

## Wie es geht

Um die Länge einer Zeichenkette in Rust zu finden, gibt es mehrere Möglichkeiten. Die einfachste ist die Verwendung der `len()` Methode, die in das `str`-Objekt integriert ist. Diese Methode kann auf jede Zeichenkette angewendet werden und gibt die Anzahl der Zeichen in der Zeichenkette zurück.

```Rust
let text = "Hallo Welt!";
let length = text.len();
println!("Die Länge der Zeichenkette ist {}", length);
```

Die Ausgabe dieses Codeschnipsels wird `Die Länge der Zeichenkette ist 11` sein, da "Hallo Welt!" 11 Zeichen enthält.

Eine andere Möglichkeit ist die Verwendung von Iteratoren, um die Länge einer Zeichenkette zu finden. Dies kann nützlich sein, wenn man bestimmte Bedingungen erfüllen oder Zeichenketten mit einer bestimmten Länge filtern möchte.

```Rust
let text = "Hallo Welt!";
let length = text.chars().count();
println!("Die Länge der Zeichenkette ist {}", length);
```

Die Methode `chars()` gibt eine Iteratorstruktur zurück, die jedes Zeichen in der Zeichenkette durchläuft. Die Methode `count()` gibt dann die Anzahl der Elemente zurück, die von diesem Iterator durchlaufen wurden. Die Ausgabe dieses Codeschnipsels ist ebenfalls `Die Länge der Zeichenkette ist 11`.

## Tiefer Einblick

Wenn man genauer betrachtet, wie Text in Rust gespeichert wird, wird es klarer, warum die beiden Methoden `len()` und `chars().count()` die gleiche Länge zurückgeben. In Rust werden Zeichenketten als UTF-8 codierte Bytes gespeichert, wobei jedes Zeichen unterschiedlich viele Bytes verwenden kann. Deshalb funktionieren die beiden oben genannten Methoden, aber es gibt auch eine dritte Methode, die `bytes().count()` genannt wird und die Anzahl der Bytes in der Zeichenkette zurückgibt, anstatt die Anzahl der Zeichen. In den meisten Fällen wird die Anzahl der Zeichen jedoch präferiert.

## Siehe auch

- Offizielle Rust Dokumentation: https://doc.rust-lang.org/book/ch08-02-strings.html
- YouTube-Tutorial zu Zeichenketten in Rust: https://www.youtube.com/watch?v=sW6xlplPwH4&ab_channel=TraversyMedia
- Beitrag zur UTF-8-Codierung in Rust: https://www.maururu.net/unicode-to-utf-8-in-rust/