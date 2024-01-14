---
title:                "Haskell: Zusammenführen von Zeichenfolgen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum
String-Konkatenation, also das Zusammenführen von Strings, ist eine wichtige grundlegende Funktion in der Programmierung. Es ermöglicht uns, Strings dynamisch zu erzeugen und zu manipulieren, was uns bei der Lösung von komplexen Problemen helfen kann.

## Wie funktioniert es
```Haskell
-- Definiere zwei Strings
let str1 = "Hallo"
let str2 = "Welt"

-- Verwende den Operator "++" zum Zusammenführen der Strings
let combinedStr = str1 ++ str2

-- Gib das Ergebnis aus
print combinedStr

-- Output: "HalloWelt"
```

In Haskell können wir die "++" Operator verwenden, um zwei oder mehrere Strings zu verbinden. Wir können auch Variablen und Zahlen in den String einfügen, indem wir sie mit dem Operator "#" umschließen. Zum Beispiel: ``` "Die Antwort ist #x" ++ 42``` würde den String "Die Antwort ist 42" erzeugen.

## Tiefergehende Informationen
String-Konkatenation wird in Haskell durch eine Operation namens "Appending" erreicht. Diese Operation kann auch verwendet werden, um andere Datentypen zu verbinden, wie zum Beispiel Listen oder Tupel. Um Strings effizient zu verbinden, verwendet Haskell eine Datenstruktur namens "Difference Lists", die Andauerndes Zusammenführen von Strings erlaubt, ohne dass dabei Performance-Probleme auftreten.

Es ist auch wichtig zu beachten, dass in Haskell Strings als eine Liste von Zeichen behandelt werden. Daher kann die Konkatenation von Strings auch durch die Operation "Concat" erreicht werden, die eine Liste von Listen akzeptiert und zu einer einzigen Liste zusammenführt.

## Siehe auch
- [String Operators in Haskell](https://wiki.haskell.org/Introduction_to_Haskell_Strings#Concatenation)
- [Difference Lists in Haskell](https://stackoverflow.com/questions/35375060/what-is-a-dlist-in-haskell)