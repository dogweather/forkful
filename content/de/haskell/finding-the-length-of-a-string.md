---
title:    "Haskell: Ermitteln der Länge eines Strings"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe, die häufig in der Programmierung verwendet wird. Es ist nützlich, um Informationen über einen String zu erhalten und kann in vielen verschiedenen Anwendungen eingesetzt werden.

## Wie gehts

Um die Länge eines Strings in Haskell zu finden, gibt es mehrere Möglichkeiten. Eine Möglichkeit ist die Verwendung der ```length``` Funktion, die in der Standardbibliothek von Haskell enthalten ist. Diese Funktion nimmt einen String als Eingabe und gibt die Anzahl der Zeichen im String zurück.

Beispiel:

```Haskell
length "Hallo Welt"
-- Output: 11
```

Eine weitere Möglichkeit ist die Verwendung der ```Data.List``` Bibliothek, die die ```genericLength``` Funktion bereitstellt. Diese Funktion kann mit verschiedenen Datentypen verwendet werden, nicht nur mit Strings. Sie verwendet jedoch die ```Num```-Klasse, um die Länge zu berechnen.

Beispiel:

```Haskell
import Data.List

genericLength "Hallo Welt"
-- Output: 11

genericLength [1,2,3]
-- Output: 3
```

## Tiefentauchen

Wenn wir uns die Implementierung der ```length``` Funktion genauer ansehen, sehen wir, dass sie rekursiv ist. Sie verwendet Musterabgleich und basiert auf der Definition der Länge eines leeren Strings als 0 und der Länge eines nicht leeren Strings als 1 plus der Länge des restlichen Teils des Strings.

Beispiel:

```Haskell
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```

Eine weitere wichtige Sache zu beachten ist, dass die ```length``` Funktion in Haskell strikt ist, was bedeutet, dass sie die Auswertung aller Elemente des Strings erfordert, anstatt nur die Länge zu bestimmen. Dies kann in einigen Fällen effizienter sein, da es sicherstellt, dass der gesamte String vollständig ausgewertet wird.

## Siehe auch

- [Haskell-Dokumentation zur length Funktion](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length)
- [Haskell-Wiki über Listenfunktionen](https://wiki.haskell.org/How_to_work_on_lists)
- [Einführung in die Haskell-Programmierung](https://www.haskell.org/tutorial/)