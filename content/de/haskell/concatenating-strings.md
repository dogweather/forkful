---
title:                "Haskell: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Zeichenketten ist eine weit verbreitete Aufgabe beim Programmieren. Es ermöglicht es, einzelne Textteile miteinander zu kombinieren und so eine größere Zeichenkette zu erstellen. Diese Funktion ist besonders nützlich, wenn es darum geht, dynamische Texte zu erzeugen, die sich je nach Situation ändern. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man in Haskell Zeichenketten verketten kann.

## Wie geht man vor?

Um Zeichenketten in Haskell zu verketten, verwenden wir die Funktion `++`. Diese Funktion nimmt zwei Zeichenketten als Argumente und gibt eine neue Zeichenkette zurück, die die beiden ursprünglichen Zeichenketten miteinander verbunden hat. Hier ist ein Beispielcode, um dies zu veranschaulichen:

```Haskell
concatString :: String -> String -> String
concatString x y = x ++ y

main = do
    let hello = "Hallo"
    let world = "Welt"
    let greeting = concatString hello world
    putStrLn greeting
```

In diesem Beispiel haben wir die Funktion `concatString` erstellt, die zwei Zeichenketten als Argumente nimmt und mithilfe von `++` zu einer neuen Zeichenkette verbindet. In der `main` Funktion haben wir dann `concatString` verwendet, um die Zeichenketten "Hallo" und "Welt" miteinander zu verbinden und das Ergebnis dann mit `putStrLn` auszugeben. Die Ausgabe wird "HalloWelt" sein.

## Tiefer Einblick

Es ist wichtig zu beachten, dass in Haskell Zeichenketten als Listen von einzelnen Zeichen behandelt werden. Die `++` Funktion arbeitet daher ähnlich wie die Funktion `++` für Listen und fügt einfach alle Elemente der zweiten Liste an die erste Liste an. Dies bedeutet auch, dass wenn wir eine leere Zeichenkette an die Funktion übergeben, erhalten wir einfach die gleiche Zeichenkette zurück.

Wir können auch mehr als zwei Zeichenketten verketten, indem wir einfach die `++` Funktion mehrmals hintereinander verwenden. Zum Beispiel:

```Haskell
main = do
    let str1 = "Hallo"
    let str2 = " "
    let str3 = "Welt"
    let str4 = "!"
    let greeting = str1 ++ str2 ++ str3 ++ str4
    putStrLn greeting
```

Dies würde die Ausgabe "Hallo Welt!" erzeugen.

## Siehe auch

- [Haskell Strings](https://www.haskell.org/tutorial/string.html)
- [Concatenating strings in Haskell](https://stackoverflow.com/questions/9064901/concatenating-strings-in-haskell)