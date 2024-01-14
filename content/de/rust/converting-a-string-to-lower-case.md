---
title:    "Rust: Ausgaben eines Strings in Kleinbuchstaben umwandeln"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man eine Programmiersprache wie Rust erlernen sollte. Einer davon ist die Fähigkeit, effizient und sicher zu programmieren. Eine häufige Aufgabe in der Programmierung ist die Konvertierung von Strings in Kleinbuchstaben. Dies kann hilfreich sein, wenn man beispielsweise Benutzereingaben in einer Datenbank speichern möchte oder bestimmte Vergleiche von Strings durchführen muss. In diesem Blog-Beitrag werden wir uns ansehen, wie man dies in Rust macht.

## Wie

Um eine Zeichenkette in Rust in Kleinbuchstaben zu konvertieren, gibt es verschiedene Möglichkeiten. Eine Möglichkeit ist die Verwendung der `to_lowercase()`-Funktion, die Teil der Standardbibliothek von Rust ist. Hier ist ein Beispiel:

```Rust
let my_string = String::from("Hallo Welt");
let lower_case_string = my_string.to_lowercase();

println!("{}", lower_case_string);

// Ausgabe: hallo welt
```

Wie man sieht, müssen wir zunächst eine Zeichenkette erstellen und dann die `to_lowercase()`-Funktion darauf anwenden. Das Ergebnis wird in `lower_case_string` gespeichert und schließlich ausgegeben. Einfach, oder?

Eine weitere Möglichkeit ist die Verwendung der `chars()`-Methode und der `map()`-Funktion. Hier ist ein Beispiel, das die gleiche Aufgabe erfüllt:

```Rust
let my_string = String::from("Hallo Welt");
let lower_case_string = my_string.chars().map(|c| c.to_lowercase().next().unwrap()).collect::<String>();

println!("{}", lower_case_string);

// Ausgabe: hallo welt
```

Bei dieser Methode wird die Zeichenkette in einzelne Zeichen aufgeteilt, dann wird `to_lowercase()` auf jedes Zeichen angewendet und schließlich werden die Zeichen wieder zu einer Zeichenkette zusammengefügt.

## Deep Dive

Wenn wir uns tiefer mit dem Konzept der Konvertierung von Zeichenketten in Rust beschäftigen wollen, können wir uns anschauen, wie Rust mit Unicode umgeht. Unicode ist ein internationaler Standard für die Codierung von Schriftzeichen. In Rust wird jeder `char`-Typ intern als UTF-8 codiert, was bedeutet, dass jedes Zeichen eine bestimmte Anzahl von Bytes belegt.

Dies hat Auswirkungen auf die Konvertierung von Zeichenketten in Kleinbuchstaben, da einige Zeichen möglicherweise mehr als ein einzelnes Byte in UTF-8 belegen. Dies kann zu Problemen führen, wenn man versucht, eine Zeichenkette in einzelne Zeichen aufzuteilen oder zu manipulieren. Deshalb verwenden wir in unseren Beispielen die `chars()`-Methode, um sicherzustellen, dass jedes Zeichen ordnungsgemäß behandelt wird.

## Siehe auch

- Offizielle Rust-Dokumentation zu `to_lowercase()`: https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase
- Weitere Informationen zu Unicode in Rust: https://doc.rust-lang.org/std/primitive.char.html#method.to_lowercase