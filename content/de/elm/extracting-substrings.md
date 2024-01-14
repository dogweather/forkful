---
title:                "Elm: Substrings extrahieren"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Substrings ist eine nützliche Funktion in Elm, die es ermöglicht, Teile von Strings basierend auf bestimmten Kriterien zu extrahieren. Dies ist besonders hilfreich, wenn man mit großen Strings arbeitet und nur bestimmte Teile davon benötigt. In diesem Blog-Beitrag werden wir genauer untersuchen, warum und wie man Substrings in Elm extrahiert.

## Anleitung

Um einen Substring in Elm zu extrahieren, können wir die `String.slice` Funktion verwenden. Diese Funktion erfordert drei Argumente: den Startindex, den Endindex und den String selbst. Der Startindex gibt an, an welcher Stelle der Substring beginnen soll, während der Endindex angibt, an welcher Stelle der Substring enden soll. Hier ist ein Beispielcode, der einen Substring von einem String extrahiert:

```Elm
-- String mit dem Text "Hello World"
let str = "Hello World"

-- Extrahieren des Substrings "World"
let substr = String.slice 6 11 str

-- Ausgabe des Substrings
-- "World"
```

Wie Sie sehen können, haben wir den Substring von Index 6 (inklusive) bis Index 11 (exklusive) extrahiert. Es ist wichtig zu beachten, dass der Endindex nicht Teil des extrahierten Substrings ist.

Wenn Sie möchten, können Sie auch negative Indizes verwenden, um Substrings zu extrahieren. Ein negativer Startindex gibt an, an welcher Stelle der letzte Zeichen des Strings als Start für den Substring verwendet werden soll, während ein negativer Endindex angibt, an welcher Stelle das vorletzte Zeichen des Strings als Ende für den Substring verwendet werden soll. Hier ist ein Beispielcode:

```Elm
-- String mit dem Text "Abcdefg"
let str = "Abcdefg"

-- Extrahieren des Substrings "def"
let substr = String.slice -5 -2 str

-- Ausgabe des Substrings
-- "def"
```

Ein weiteres nützliches Feature ist die Möglichkeit, den Endindex weglassen zu können. Dadurch wird automatisch der Rest des Strings vom angegebenen Startindex bis zum Ende als Substring extrahiert. Hier ist ein Beispielcode:

```Elm
-- String mit dem Text "123456789"
let str = "123456789"

-- Extrahieren des Substrings "456789"
let substr = String.slice 3 str

-- Ausgabe des Substrings
-- "456789"
```

## Tiefergehende Informationen

Es gibt noch weitere Funktionen, die beim Extrahieren von Substrings in Elm hilfreich sein können. Die `String.left` Funktion gibt eine bestimmte Anzahl von Zeichen vom Anfang des Strings zurück, während `String.right` eine bestimmte Anzahl von Zeichen vom Ende des Strings zurückgibt. Es ist auch möglich, einen bestimmten String als Trennzeichen zu verwenden, um den String in Unterstrings zu teilen, indem die Funktion `String.split` verwendet wird.

Es ist wichtig zu beachten, dass das Extrahieren von Substrings nicht nur auf Strings beschränkt ist. Auch in Listen kann man Sublists extrahieren, indem man die `List.slice` Funktion verwendet.

## Siehe auch

- [`String.slice` Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- ["Startseite" einer deutschen Elm-Community](https://elm-lang.de/)