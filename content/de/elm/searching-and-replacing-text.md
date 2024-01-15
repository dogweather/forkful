---
title:                "Suchen und Ersetzen von Text"
html_title:           "Elm: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Hast du jemals eine lange Liste von Texten durchsucht, um bestimmte Wörter oder Ausdrücke zu finden und zu ersetzen? Das kann sehr mühsam und zeitaufwändig sein. Zum Glück gibt es in Elm eine eingebaute Funktion, die dir helfen kann, diesen Prozess zu automatisieren.

## So geht's

Um Text in Elm zu suchen und zu ersetzen, kannst du die Funktion `String.replace` verwenden. Sie erwartet drei Argumente: den zu ersetzenden Text, den Ersatztext und den ursprünglichen Text.

```Elm
import String

originalText = "Das ist ein Beispieltext mit dem Wort 'Lorem'"
ersatzText = "Dieser Satz wurde geändert"
zuErsetzenderText = "Lorem"

String.replace zuErsetzenderText ersatzText originalText -- Ergebnis: "Das ist ein Beispieltext mit dem Wort 'Dieser Satz wurde geändert'"
```

Der zu ersetzende Text kann auch ein regulärer Ausdruck sein, so dass du noch flexibler beim Ersetzen von Wörtern oder Ausdrücken bist. Hier ist ein Beispiel:

```Elm
import String.Regex

originalText = "Ich habe an diesem Tag 10 Äpfel gegessen, aber niemand glaubt mir"
ersatzText = "Ich hatte an diesem Tag 0 Äpfel gegessen"
zuErsetzenderText = "\\d+"
regex = String.Regex.fromString zuErsetzenderText

String.replace regex ersatzText originalText -- Ergebnis: "Ich hatte an diesem Tag 0 Äpfel gegessen, aber niemand glaubt mir"
```

Es ist auch möglich, mehrere Wörter oder Ausdrücke gleichzeitig zu ersetzen, indem du die `replace`-Funktion innerhalb der `List.foldl`-Funktion verwendest.

```Elm
import List exposing (foldl)
import String

originalText = "Dieser Text ist ein gutes Beispiel für eine Übung"
ersatzPaare = [ ("gutes", "schlechtes"), ("Übung", "Spiel") ]

String.foldl (\(zuErsetzenderText, ersatzText) text -> String.replace zuErsetzenderText ersatzText text) originalText ersatzPaare 

-- Ergebnis: "Dieser Text ist ein schlechtes Beispiel für ein Spiel"
```

## Tiefer Einblick

Die `replace`-Funktion gibt den ursprünglichen Text zurück, wenn der zu ersetzende Text nicht gefunden wurde. Um dies zu vermeiden und sicherzustellen, dass alle Instanzen des zu ersetzenden Textes ersetzt werden, solltest du die `map`-Funktion innerhalb der `List`-Module verwenden.

Außerdem ist es auch möglich, zusätzlich zu den drei erwarteten Argumenten ein viertes Argument anzugeben, das die maximale Anzahl der zu ersetzenden Instanzen festlegt.

Weitere Funktionen und Optionen zum Suchen und Ersetzen von Text in Elm findest du in der offiziellen Elm-Dokumentation.

## Siehe auch

- [Offizielle Elm-Dokumentation zu String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Offizielle Elm-Dokumentation zu List](https://package.elm-lang.org/packages/elm/core/latest/List)