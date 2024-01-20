---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Musterlöschung ist das Löschen von Zeichen, die einer bestimmten Regel oder einem Muster entsprechen. Programmierer nutzen diese Technik, um unnötige oder störende Daten aus Texten zu entfernen.

## So geht's:

Um Zeichen in Elm zu entfernen, die einem Muster entsprechen, verwenden wir die Funktion `String.replace`. Hier sehen Sie, wie Sie alle Vorkommen des Buchstabens 'a' aus einer Zeichenkette entfernen:

```Elm
import String

removeCharacter : String -> String
removeCharacter s = 
    String.replace "a" "" s

main = 
    removeCharacter "Banana" |> Debug.toString
    -- Output: "Bnn"
```

## Tiefer Einblick

Die Möglichkeit, Muster in einer Zeichenkette zu löschen, reicht bis in die frühesten Tage der Textverarbeitung zurück, als es wichtig war, maschinell lesbare Daten von Rauschen zu trennen. Es gibt mehrere Alternativen zum Löschen von Mustern, einschließlich regulärer Ausdrücke, die jedoch von Elm bewusst nicht unterstützt werden, um die einfache Arbeitsweise und Ausführungsgeschwindigkeit des Codes zu verbessern. Bei der Implementierung beachten Sie, dass `String.replace` in Elm jedes Vorkommnis des Musters in der Zeichenkette ersetzt.

## Siehe auch

- [Offizielle Elm-Dokumentation über String.replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Elm-Diskussionsforum zur Bearbeitung von Zeichenketten](https://discourse.elm-lang.org/t/simple-string-manipulation/6478)