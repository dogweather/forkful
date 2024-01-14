---
title:                "Elm: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung sehr nützlich sein, insbesondere wenn man mit umfangreichen Texten arbeitet. Mit Elm ist es möglich, diese Aufgabe auf einfache und effiziente Weise zu bewältigen.

## Wie funktioniert es?

Um Zeichen zu löschen, die einem bestimmten Muster entsprechen, müssen wir zuerst einen regulären Ausdruck definieren, der dieses Muster beschreibt. Anschließend können wir die `Regex.replace` Funktion verwenden, um alle Vorkommen des Musters im Text zu entfernen.

Ein Beispielcode könnte wie folgt aussehen:

```Elm
import Regex

text = "Hello, world! I am learning Elm."
pattern = Regex.regex "[A-Z]"

Regex.replace pattern text ""
```

Der obige Code würde alle Großbuchstaben aus dem Text löschen und das Ergebnis würde wie folgt aussehen:

`"ello, world! am earning lm."`

Mehr Beispiele und Informationen zu Regulären Ausdrücken in Elm finden Sie in der [offiziellen Dokumentation](https://elm-lang.org/docs/quick-start).

## Tiefergehende Erläuterung

Beim Löschen von Zeichen in Elm gibt es einige wichtige Dinge zu beachten. Zum einen beruht die Funktion `Regex.replace` auf dem `Maybe`-Typ, was bedeutet, dass sie möglicherweise kein Ergebnis zurückgibt, wenn zum Beispiel der verwendete reguläre Ausdruck nicht auf den Text zutrifft.

Zum anderen können wir mit dem `group` Argument in der `Regex.replace` Funktion angeben, welcher Teil des regulären Ausdrucks wir im Endergebnis beibehalten möchten. Dadurch können wir gezielt bestimmte Zeichen aus unserem Text entfernen und den Rest beibehalten.

Für eine genauere Erklärung und weitere Beispiele empfehlen wir einen Blick in das [Elm Guide Buch](https://guide.elm-lang.org/) zu werfen.

## Siehe auch

- [Offizielle Elm Dokumentation](https://elm-lang.org/docs)
- [Elm Guide Buch](https://guide.elm-lang.org/)
- [Regex in Elm - Traversy Media Tutorial](https://www.youtube.com/watch?v=00EYJx6iADI) (auf Englisch)