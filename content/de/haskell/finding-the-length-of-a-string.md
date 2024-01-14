---
title:                "Haskell: Die Länge eines String finden"
simple_title:         "Die Länge eines String finden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Funktion, die bei der Verarbeitung von Textdaten unerlässlich ist. Es ermöglicht uns, die Größe des Textes zu bestimmen und ihn entsprechend zu manipulieren.

## Wie es geht

Um die Länge eines Strings in Haskell zu finden, können wir die vordefinierten Funktionen `length` oder `Data.Text.length` verwenden. Hier ein Beispiel:

```Haskell
-- Input String
text = "Das ist ein Beispieltext"

-- Verwendung der Funktion length
print (length text) --> 23

-- Verwendung der Funktion Data.Text.length
import Data.Text (length)
print (length text) --> 23
```

Wir können auch eine eigene Funktion schreiben, um die Länge eines Strings zu finden. Hier ist ein Beispiel, das die Rekursion verwendet:

```Haskell
-- Definieren einer Funktion zur Findung der Länge eines Strings
lengthCustom :: String -> Int
lengthCustom [] = 0 -- Basisfall
lengthCustom (x:xs) = 1 + lengthCustom xs

-- Aufruf der Funktion
print (lengthCustom text) --> 23
```

## Tiefeneintauchen

Die Funktion `length` gibt uns die Anzahl der Buchstaben im String zurück. Wenn wir jedoch die Anzahl der Wörter in einem String finden möchten, müssen wir etwas zusätzliche Arbeit leisten. Eine Möglichkeit ist, den String in eine Liste von Wörtern zu zerlegen und dann die Länge dieser Liste zu finden.

```Haskell
-- Definieren einer Funktion zur Findung der Anzahl der Wörter in einem String
countWords :: String -> Int
countWords text = length (words text)

-- Aufruf der Funktion
print (countWords text) --> 4
```

## Siehe auch

- [Haskell Crashkurs für Anfänger](https://medium.com/de-de/free-code-camp/haskell-5-minuten-crashkurs-f%C3%BCr-anf%C3%A4nger-98b53d0abff4)
- [Offizielle Haskell-Dokumentation](https://www.haskell.org/documentation/)
- [String Verarbeitung in Haskell](https://www.haskell.org/tutorial/strings.html)