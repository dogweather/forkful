---
title:    "Elm: Übereinstimmenden Mustern entsprechende Zeichen löschen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung nützlich sein, um unerwünschte oder redundante Daten zu entfernen oder eine Formatierung zu ändern.

## Wie geht das?

Die Elm-Sprache bietet verschiedene Funktionen und Methoden, um das Löschen von Zeichen basierend auf einem Muster zu erleichtern. Hier sind ein paar Beispiele:

```elm
-- Lösche alle Leerzeichen aus einem String
removeSpaces : String -> String
removeSpaces string =
  String.filter (\c -> c /= ' ') string

-- Lösche alle Zahlen aus einem String
removeNumbers : String -> String
removeNumbers string =
  String.filter (\c -> not (Char.isDigit c)) string

```

Der obige Code verwendet die Funktion `String.filter`, um ein Muster abzugleichen und alle passenden Zeichen zu entfernen. In diesem Fall werden alle Leerzeichen bzw. Zahlen aus dem ursprünglichen String entfernt.

In Elm gibt es auch die Funktion `String.split`, die einen String in eine Liste von Strings aufteilt, basierend auf einem Trennzeichen. So kannst du zum Beispiel alle Wörter aus einem Satz einlesen und dann die gewünschten Zeichen löschen:

```elm
-- Lösche alle Vokale aus einem Satz
removeVowels : String -> String
removeVowels string =
  string
    |> String.words
    |> List.map (\word -> String.filter (\c -> not (String.contains "aeiou" c)) word)
    |> String.join " "
```

Dieser Code verwendet die Funktion `String.words`, um den Satz in eine Liste von Wörtern aufzuteilen. Anschließend geht er jedes Wort durch und entfernt alle Vokale durch Verwendung von `String.filter`. Schließlich werden die verbleibenden Wörter wieder zu einem String zusammengefügt.

## Tiefer Einblick

Das Löschen von Zeichen basierend auf einem Muster ist ein häufiges Problem in der Programmierung und es gibt viele verschiedene Ansätze, um dies zu lösen. In Elm gibt es nicht nur die oben genannten Funktionen, sondern auch andere nützliche Methoden wie `String.map`, `String.dropLeft`, `String.dropRight` und mehr. Es ist wichtig, die richtige Funktion für den jeweiligen Anwendungsfall zu wählen, um eine effiziente und saubere Lösung zu erreichen.

## Siehe auch

- Elm Dokumentation zu Strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Artikel über die Verwendung von Strings in Elm: https://dev.to/lydiahallie/strings-in-elm-218
- Offizieller Guide zur Elm-Sprache: https://guide.elm-lang.org/