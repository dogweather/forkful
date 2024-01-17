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

## Was & Warum?

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe für Programmierer. Es bezieht sich auf das Auffinden bestimmter Zeichenfolgen in einem Text und das Ersetzen dieser durch eine andere Zeichenfolge. Dies kann nützlich sein, um Fehler zu korrigieren, Text zu formatieren oder große Mengen an Daten schnell zu bearbeiten.

## Wie geht's?

Der Prozess des Suchens und Ersetzens von Text in Elm ist relativ einfach und intuitiv. Hier ist ein Beispiel, wie man alle Vorkommen des Wortes "Hallo" im Text durch das Wort "Guten Tag" ersetzt:

```
Elm String.replace "Hallo" "Guten Tag" "Hallo Welt!" -- Output: "Guten Tag Welt!"
```

Man kann auch reguläre Ausdrücke verwenden, um komplexere Suchanfragen zu erstellen. Zum Beispiel, um Zahlen im Text aufzuspüren und durch einen Platzhalter zu ersetzen, kann man Folgendes verwenden:

```
Elm Regex.replace (Regex.regex "\\d+") (always "###") "Die Antwort ist 42." -- Output: "Die Antwort ist ###."
```

## Tiefer tauchen

Das Konzept des Suchens und Ersetzens von Text hat eine lange Geschichte und wurde in verschiedenen Programmiersprachen und Tools implementiert. Einige alternative Möglichkeiten, dies in Elm zu tun, sind die Verwendung von Regex-Bibliotheken wie [elm-regex](https://package.elm-lang.org/packages/elm-community/regex/latest/) oder das Pattern Matching mit benutzerdefinierten Funktionen.

Elm bietet auch eine Suche und Ersetzen Funktion für Listen, die es ermöglicht, ein bestimmtes Element in einer Liste zu finden und durch ein anderes zu ersetzen. Diese Funktion heißt `List.map` und kann ebenfalls für das Suchen und Ersetzen von Text verwendet werden.

Eine wichtige Implementierungsdetails zu beachten ist, dass die `replace` Funktion in Elm eine vollständige Textsuche und kein startIndex-Parameter benötigt, was bedeutet, dass alle Vorkommen des Suchmusters im Text ersetzt werden.

## Siehe auch

- [Offizielle Elm Dokumentation](https://guide.elm-lang.org/)
- [Reguläre Ausdrücke in Elm](https://dev.to/noahfrey/regular-expressions-in-elm-56dh)
- [Alternative Suche und Ersetzen Methoden in Elm](https://www.reddit.com/r/elm/comments/2awz04/alternatives_to_replace/)