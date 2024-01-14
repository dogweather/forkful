---
title:                "Elm: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings, also Teilen eines längeren Strings, kann sehr nützlich sein, um spezifische Informationen aus einem Text zu erhalten. In Elm gibt es verschiedene Funktionen, die dabei helfen können, diese Aufgabe effizient zu erledigen.

## Wie funktioniert es?

Um Teilstrings in Elm zu extrahieren, gibt es zwei Hauptfunktionen: `slice` und `substring`. Die `slice`-Funktion erwartet den Start- und Endindex des zu extrahierenden Teilstrings sowie den ursprünglichen String. Zum Beispiel:

```Elm
originalString = "Bankkonto"
teilstring = slice 0 4 originalString
```

In diesem Beispiel wird der Teilstring "Bank" aus dem ursprünglichen String extrahiert und der Variablen `teilstring` zugewiesen. Das Ergebnis wäre also `"Bank"`.

Eine andere Möglichkeit, Teilstrings zu extrahieren, ist die `substring`-Funktion. Sie erwartet ebenfalls den Start- und Endindex, jedoch mit dem Unterschied, dass der Endindex nicht einschließlich, sondern exklusive ist. Zum Beispiel:

```Elm
originalString = "Laptop"
teilstring = substring 2 5 originalString
```

Hier würde der Teilstring "pto" extrahiert werden. Wie man sieht, ist der Startindex inklusive ("top" beginnt an Index 2), der Endindex jedoch exklusive ("to" endet an Index 5).

## Tiefere Einblicke

Es gibt noch weitere Funktionen, um Teilstrings in Elm zu extrahieren, wie z.B. `left` und `right`. Diese ermöglichen das Extrahieren von Teilstrings ab einem bestimmten Index bis zum Anfang oder Ende des ursprünglichen Strings. Es gibt auch die Funktion `trim`, die Leerzeichen vor und/oder nach dem Teilstring entfernt.

Es ist auch möglich, Teilstrings mit Hilfe von Bedingungen zu extrahieren. Hierfür gibt es die `contains`-Funktion, die überprüft, ob ein bestimmter String in einem anderen String enthalten ist und dann den entsprechenden Teilstring extrahiert.

## Siehe auch

* [Offizielle Elm-Dokumentation zur String-Extraktion](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
* [Elm Cheat Sheet zur String-Extraktion](https://elmprogramming.com/elm-cheat-sheet.pdf) 
* [Blog-Beitrag zu nützlichen String-Funktionen in Elm](https://www.brianthicks.com/post/2018/08/07/some-useful-elm-string-functions/)