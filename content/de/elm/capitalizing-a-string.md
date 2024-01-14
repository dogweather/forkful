---
title:    "Elm: String in Großbuchstaben umwandeln"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist eine grundlegende Operation bei der Entwicklung von Software. Es kann verwendet werden, um die Lesbarkeit von Benutzereingaben zu verbessern oder um ein bestimmtes Format in den Ausgaben zu erreichen.

## Wie

Das Kapitalisieren eines Strings in Elm ist sehr einfach. Wir können die Funktion `String.toUpper` verwenden, um den gesamten String in Großbuchstaben zu konvertieren. Hier ist ein Beispielcode:

```Elm
String.toUpper "Hallo Welt!" -- Gibt "HALLO WELT!" aus
```

Alternativ können wir die Funktion `String.toTitle` verwenden, um nur den Anfangsbuchstaben jedes Wortes in einem String zu kapitalisieren. Hier ist ein Beispielcode:

```Elm
String.toTitle "hallo welt!" -- Gibt "Hallo Welt!" aus
```

## Deep Dive

Für diejenigen, die tiefer in die Details des Kapitalisierens eines Strings in Elm eintauchen möchten, gibt es einige wichtige Dinge zu beachten. Zum Beispiel sind `String.toUpper` und `String.toTitle` nicht sehr effizient für Strings mit mehr als 6 Zeichen. Für längere Strings kann die Verwendung von `String.foldl` und `Char.toUpper` eine bessere Option sein. Außerdem kann es schwierig sein, Strings in anderen Sprachen als Englisch zu kapitalisieren. Für solche Fälle könnte die Verwendung des Online-Services "Unicode String Capitalizer" von der Firma Elixa oder die Entwicklung einer eigenen benutzerdefinierten Funktion erforderlich sein.

## Siehe auch

- [Offizielle Elm Dokumentation: String Modul](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode String Capitalizer von Elixa](https://www.elixa.com/tools/unicode-capitalization.htm)
- [Offizielle Elm Dokumentation: Char Modul](https://package.elm-lang.org/packages/elm/core/latest/Char)