---
title:                "Rust: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Welt der Softwareentwicklung gibt es unzählige Programmiersprachen zur Auswahl. Eine der neueren Sprachen, die jedoch schnell an Popularität gewinnt, ist Rust. Rust ist eine performante und sichere Systemsprache, die dafür bekannt ist, fehleranfällige Programme zu vermeiden. Eines der nützlichen Features von Rust ist die Möglichkeit, Substrings aus Strings zu extrahieren. Aber warum sollte man das überhaupt tun?

Die Extraktion von Substrings ist eine häufige Aufgabe in der Programmierung. Sie kann nützlich sein, um Teile eines Strings zu isolieren, um sie weiter zu verarbeiten oder zu überprüfen. Dies kann besonders hilfreich sein, wenn man mit Benutzer*innen-Eingaben interagiert oder bestimmte Muster in einem Text finden möchte.

## Wie geht das?

Um Substrings in Rust zu extrahieren, gibt es zwei Hauptmethoden: die `slice`-Methode und die `split`-Methode. Die `slice`-Methode ermöglicht es uns, einen Teil eines Strings basierend auf der Anzahl der Zeichen zu extrahieren, während die `split`-Methode es uns ermöglicht, einen String anhand eines bestimmten Zeichens oder Musters in mehrere Substrings aufzuteilen.

Schauen wir uns einige Beispiele an:

```Rust
let text = "Dies ist ein Beispieltext.";
// Verwende die slice-Methode, um die ersten 4 Zeichen zu extrahieren
let first_four = &text[0..4]; // "Dies"
// Verwende die split-Methode, um den Text in mehrere Substrings aufzuteilen
let substrings = text.split(" "); // ["Dies", "ist", "ein", "Beispieltext."]
```

Wie im Beispiel zu sehen ist, können wir mit der `slice`-Methode einen Teil eines Strings auswählen, indem wir den Index des ersten und letzten Zeichens angeben, und mit der `split`-Methode den Text anhand des Leerzeichens in mehrere Substrings aufteilen.

Es ist auch möglich, einigen Zusatzfunktionen wie `trim` oder `to_lowercase` anzuwenden, um den extrahierten Substring weiter anzupassen.

## Tiefer ins Detail gehen

Die `slice`- und `split`-Methoden sind die grundlegenden Methoden zum Extrahieren von Substrings in Rust. Es gibt jedoch auch weitere Funktionen und Bibliotheken, die es einem ermöglichen, komplexere Substrings-Extraktionen durchzuführen.

Eine dieser Bibliotheken ist `regex`, mit der man mithilfe von regulären Ausdrücken genau bestimmte Patterns in einem Text finden kann. Eine andere nützliche Funktion ist die `chars`-Methode, mit der man auf einzelne Zeichen eines Strings zugreifen kann.

Es ist wichtig zu beachten, dass die Extraktion von Substrings in Rust im Gegensatz zu anderen Sprachen, wie z.B. Java oder Python, etwas anders funktioniert. Durch die Verwendung von Borrowing und Slices kann es zu keinen unerwarteten Seiteneffekten oder Performanceproblemen kommen.

## Siehe auch

- [Rust-Dokumentation über Strings](https://doc.rust-lang.org/std/string/index.html)
- [Offizielle Rust-Website](https://www.rust-lang.org/)
- [Einführung in Rust](https://www.rust-lang.org/learn)