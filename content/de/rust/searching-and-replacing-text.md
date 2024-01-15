---
title:                "Suchen und Ersetzen von Text"
html_title:           "Rust: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon einmal auf der Suche nach einem bestimmten Text in einer Datei oder einem Dokument warst und ihn dann durch einen anderen Text ersetzen wolltest, dann kennst du vielleicht das lästige Gefühl, jeden einzelnen Abschnitt manuell zu ändern. Aber zum Glück gibt es eine elegantere Lösung - das Suchen und Ersetzen mit Rust.

## Wie geht's

Das Suchen und Ersetzen von Text in Rust ist sehr einfach und effizient. Zunächst brauchst du ein Textdokument oder eine Datei, in der du den Text ersetzen möchtest. Dann kannst du die Funktion `replace()` verwenden. Hier ist ein Beispielcode:

```Rust
let text = "Hallo Welt!";
let new_text = text.replace("Welt", "Rust");
println!("{}", new_text);
```
Die Ausgabe wäre: `Hallo Rust!`.

Du kannst auch mehrere Textabschnitte auf einmal ersetzen, indem du einen Vektor mit Tupeln an die `replace()` Funktion übergibst. Schau dir dieses Beispiel an:

```Rust
let text = "Renz und Rollo haben eine Reise gemacht.";
let replacements = vec![("Renz", "Alex"), ("Rollo", "Ben")];
let new_text = text.replace(replacements);
println!("{}", new_text);
```
Die Ausgabe wäre: `Alex und Ben haben eine Reise gemacht.`.

Das Suchen und Ersetzen von Text ist also sehr einfach und flexibel in Rust.

## Tiefer Einblick

Die `replace()` Funktion verwendet das Modul `std::string::String`, das Teil der Standardbibliothek von Rust ist. Diese Funktion akzeptiert entweder einen einzelnen Such- und Ersatztext oder einen Vektor von Tupeln, wobei jedes Tupel aus dem zu suchenden Text und dem Ersatztext besteht. Wenn der Suchtext nicht gefunden wird, bleibt der ursprüngliche Text unverändert.

Außerdem gibt es in Rust auch das Modul `std::fs`, das erweiterte Funktionen für das Arbeiten mit Textdateien bietet. So kannst du z.B. eine Datei öffnen, den Text darin ersetzen und dann die Datei wieder speichern. Dies ist besonders nützlich, wenn du große Textdateien bearbeitest.

## Siehe auch

- [Die offizielle Rust Dokumentation](https://doc.rust-lang.org/std/fs/)
- [Tutorials und Beispiele für das Suchen und Ersetzen in Rust](https://www.reddit.com/r/rust/comments/60nibu/searching_and_replacing_text_in_rust/)