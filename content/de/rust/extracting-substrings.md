---
title:                "Rust: Extrahieren von Teilstrings"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

In Rust sind Strings standardmäßig immutable, was bedeutet, dass sie nach der Initialisierung nicht mehr verändert werden können. Aber manchmal möchten wir vielleicht nur einen Teil eines Strings nutzen, anstatt den ganzen String zu durchlaufen. Hier kommt das Extrahieren von Substrings ins Spiel.

## Wie geht das?

Um Substrings in Rust zu extrahieren, können wir die `substring` Methode verwenden. Diese Methode nimmt zwei Parameter an: den Startindex (inklusiv) und den Endindex (exklusiv) des Substrings. Schauen wir uns ein Beispiel an:

```Rust
let string = "Hallo, Welt!";
let substring = string.substring(6, 10);

println!("{}", substring);
```

In diesem Beispiel wird der Substring von Index 6 (inklusiv) bis Index 10 (exklusiv), also "Welt", extrahiert und ausgegeben. Der resultierende Output wäre also "Welt".

## Tieferer Einblick

Das Extrahieren von Substrings in Rust kann auch mit Hilfe von Slices erfolgen. Slices sind Teilbereiche eines Arrays oder Vectors, die sich im Speicher auf einen bestimmten Abschnitt beziehen. Um einen Slice zu erstellen, verwenden wir die Syntax `&String[startindex..endindex]`.

Wir können auch negative Indizes verwenden, um von rechts nach links zu zählen, wobei -1 dem letzten Index entspricht. Wenn wir einen Slice mit negativem Startindex erstellen, zählt Rust von hinten und extrahiert den Teil ab dem angegebenen Index bis zum Ende des Strings.

```Rust
let string = "Hallo, Welt!";
let substring = &string[6..];

println!("{}", substring);
```

Dieses Beispiel würde den Substring "Welt!" extrahieren und ausgeben.

## Siehe auch

- [Rust Dokumentation über Strings](https://doc.rust-lang.org/std/string/index.html)
- [Tutorial zu Slices in Rust](https://doc.rust-lang.org/book/ch04-03-slices.html)