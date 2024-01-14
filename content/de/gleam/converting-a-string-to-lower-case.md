---
title:    "Gleam: Umwandeln eines Strings in Kleinbuchstaben"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Zeichenketten in Kleinbuchstaben kann nützlich sein, wenn man sicherstellen möchte, dass alle Buchstaben in einer Zeichenkette einheitlich sind. Zum Beispiel, wenn man nach bestimmten Wörtern in einer Textdatei sucht, kann es hilfreich sein, alle Buchstaben in Kleinbuchstaben umzuwandeln, um sicherzustellen, dass keine Suchergebnisse aufgrund von Groß- und Kleinbuchstabenabweichungen verpasst werden.

## Wie man es macht

Eine einfache Möglichkeit, eine Zeichenkette in Kleinbuchstaben umzuwandeln, ist die Verwendung der Funktion `String.to_lower()` in Gleam. Hier ist ein Beispiel:

```Gleam
// Deklariere eine Zeichenkette
let string = "GLEAM PROGRAMMING"

// Wende die Funktion an
let lowercase_string = String.to_lower(string)

// Gebe das Ergebnis aus
IO.inspect(lowercase_string)
```

Dies wird `gleam programming` als Ergebnis ausgeben.

## Tiefer Einblick

In Gleam wird das Konvertieren einer Zeichenkette in Kleinbuchstaben durch die Verwendung der Unicode-Kategorie "Ll" (Lowercase Letter) Algorithmus des Unicode-Konsortiums erreicht. Dies bedeutet, dass alle Unicode-Buchstaben, die zur Kategorie "Ll" gehören, in Kleinbuchstaben umgewandelt werden, während andere Zeichen unverändert bleiben. Dies erlaubt es, auch nicht lateinische Zeichen in Kleinbuchstaben umzuwandeln, was in manchen Programmiersprachen problematisch sein kann.

## Siehe auch

- [Gleam Dokumentation über Zeichenketten](https://gleam.run/book/core_library/strings.html)
- [Unicode Konsortium](https://www.unicode.org/)
- [Liste der Unicode Kategorien](https://www.unicode.org/reports/tr44/tr44-24.html#GC_Values_Table)