---
title:                "Haskell: Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann nützlich sein, wenn man bestimmte Daten filtern oder formatieren möchte.

## Wie funktioniert es

Die Funktion `filter` in Haskell kann verwendet werden, um eine Liste von Zeichen zu durchlaufen und Zeichen zu löschen, die einem bestimmten Muster entsprechen. Hier ist ein Beispielcode, der alle Vokale aus einem Wort löscht:

```Haskell
filter (\x -> notElem x "aeiou") "Hallo"
```

Die Ausgabe wäre dann `"Hll"`.

## Tiefergehende Informationen

In Haskell gibt es verschiedene Möglichkeiten, Zeichen aus einer Liste zu filtern. Die oben genannte Methode ist nur eine davon. Man kann auch reguläre Ausdrücke verwenden oder eine eigene Funktion schreiben, die auf spezifische Zeichen eines Musters überprüft. Mit der richtigen Kombination aus Funktionen und Mustern kann man sehr flexibel Zeichen löschen.

## Siehe auch

- [Die offizielle Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Ein Tutorial zu Listen in Haskell](https://wiki.haskell.org/Introduction_to_Lists)
- [Ein nützlicher Blog Post zum Thema Zeichenfilterung in Haskell](https://blog.jakuba.net/2014/06/11/filtering-characters-in-haskell/)