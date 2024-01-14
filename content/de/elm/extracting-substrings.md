---
title:    "Elm: Unterstrings extrahieren"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum?

Warum sollte man sich überhaupt mit dem Extrahieren von Teilstrings beschäftigen? Nun, es gibt viele Gründe - vielleicht möchtest du bestimmte Teile eines Textes isolieren, um sie zu analysieren, zu formatieren oder zu manipulieren. Vielleicht möchtest du auch einfach nur ein Wort oder eine Phrase hervorheben, um sie in einer anderen Anwendung zu verwenden. Das Extrahieren von Teilstrings ist eine wichtige Fähigkeit, die dir dabei helfen kann, deine Elm Programme noch effizienter zu gestalten.

## Wie geht's?

Um Teilstrings in Elm zu extrahieren, kannst du die Funktion `String.slice` verwenden. Diese Funktion akzeptiert drei Argumente: den Startindex, den Endindex und den String, aus dem der Teilstring extrahiert werden soll. Hier ist ein einfaches Beispiel:

```Elm
import String

myString = "Hallo Welt"

partOfString = String.slice 0 5 myString
-- partOfString = "Hallo"
```

Du kannst auch negative Indizes verwenden, um die Teilstrings vom Ende des Strings aus zu extrahieren. Hier ist ein Beispiel:

```Elm
import String

myString = "Willkommen im Elm Universum"

partOfString = String.slice -9 0 myString
-- partOfString = "Universum"
```

Mehrere Teilstrings können auch hintereinander extrahiert werden, indem man eine Liste von Indizes anstatt eines einzelnen Index verwendet. Hier ist ein Beispiel:

```Elm
import String

myString = "1,2,3,4,5,6,7,8,9,10"

partOfStrings = String.slice [2,5,8] 9 myString
-- partOfStrings = ["2", "5", "8"]
```

## Tiefere Einblicke

Das Extrahieren von Teilstrings mag auf den ersten Blick einfach erscheinen, aber es gibt einige Dinge zu beachten. Zum Beispiel, wenn du versuchst, einen Teilstring mit einem negativen Startindex zu extrahieren, musst du sicherstellen, dass der Endindex größer als der Startindex ist, sonst erhältst du einen leeren String als Ergebnis. Hier ist ein Beispiel:

```Elm
import String

myString = "Ist das nicht verwirrend?"

partOfString = String.slice -1 5 myString
-- partOfString = ""
```

Es ist auch wichtig, sicherzustellen, dass deine Indizes innerhalb der Länge des Strings bleiben, sonst kann es zu einem Fehler kommen. Hier ist ein Beispiel:

```Elm
import String

myString = "Ich bin ein langer String"

partOfString = String.slice 0 35 myString
-- ERROR: Index 35 is larger than the length of the string (length: 25)
```

## Siehe auch

* [Offizielle Dokumentation zu String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
* [Weitere nützliche String-Funktionen in Elm](https://ckeditor.com/blog/Elm-String-Basics/)
* [Verschiedene Anwendungsbeispiele für das Extrahieren von Teilstrings in Elm](https://www.samos-it.com/Elm/Views/Stringsessions.html#String[]
)# Siehe auch

* [Offizielle Dokumentation zu String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
* [Weitere nützliche String-Funktionen in Elm](https://ckeditor.com/blog/Elm-String-Basics/)
* [Verschiedene Anwendungsbeispiele für das Extrahieren von Teilstrings in Elm](https://www.samos-it.com/Elm/Views/Stringsessions.html#String[])