---
title:                "Gleam: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Text zu suchen und zu ersetzen ist eine grundlegende Fähigkeit beim Programmieren. Es ermöglicht uns, schnell und effektiv Fehler zu korrigieren oder bestimmte Wörter oder Zeichenfolgen in unserem Code zu ändern. Es ist auch besonders hilfreich, wenn wir denselben Code in verschiedenen Stellen wieder verwenden möchten. Daher ist es wichtig, dass wir als Programmierer wissen, wie man Texte suchen und ersetzen kann.

## Wie es geht

Das Gleam-Standardbibliothek bietet verschiedene Funktionen, um Text zu suchen und zu ersetzen. Hier sind zwei Beispiele:

```Gleam
let ergebnis = Text.replace("Hallo Welt", "Hallo", "Guten Tag")
// Ergebnis: "Guten Tag Welt"

let zahlen = Text.replace("1,2,3,4,5", ",", " ")
// Ergebnis: "1 2 3 4 5"
```

In diesen Beispielen nutzen wir die `replace` Funktion, die drei Argumente akzeptiert - den ursprünglichen Text, den zu ersetzenden Text und den Ersatz. Sie können auch eine Funktion angeben, die entscheidet, welcher Teil des Textes ersetzt werden soll. Diese können wir in einem zusätzlichen Argument angeben:

```Gleam
let ergebnis = Text.replace_with("Hallo Welt", "Hallo", |match| {
  if match == "Hallo" {
    "Guten Tag"
  } else {
    match
  }
})
// Ergebnis: "Guten Tag Welt"
```

Dieses Beispiel zeigt eine Funktion, die entscheidet, dass nur der Text "Hallo" ersetzt werden soll, während alles andere erhalten bleibt.

## Tiefe Tauchen

Es gibt noch weitere Funktionen in der Gleam-Standardbibliothek, die uns beim Suchen und Ersetzen von Text helfen können. Unter anderem gibt es Funktionen wie `find`, `contains`, `first_match`, `matches`, die es uns ermöglichen, Teile des Textes zu finden, die mit bestimmten Kriterien übereinstimmen. Auch können wir reguläre Ausdrücke verwenden, um noch komplexere Suchanfragen zu erstellen.

Ein wichtiger Aspekt beim Suchen und Ersetzen von Text ist die Leistung. Durch die Verwendung von Algorithmen wie Boyer-Moore oder Knuth-Morris-Pratt kann die Suche nach Text in Millionen von Zeichen sehr schnell erfolgen. Daher ist es wichtig, dass wir uns mit diesen Algorithmen vertraut machen, um unsere Programmierfähigkeiten weiter zu optimieren.

## Siehe auch

- [Gleam-Standardbibliothek](https://gleam.run/) - Offizielle Gleam-Dokumentation
- [Einführung in die Textverarbeitung mit Gleam](https://gleam.run/articles/text-processing) - Tutorial zur Verwendung von Textverarbeitungsfunktionen in Gleam
- [Einführung in reguläre Ausdrücke](https://gleam.run/articles/regex) - Artikel über die Verwendung von regulären Ausdrücken in Gleam.