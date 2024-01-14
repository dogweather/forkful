---
title:                "Haskell: Die Länge eines Strings berechnen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings kann ein nützlicher Schritt sein, wenn man mit Textverarbeitung in Haskell programmiert. Es ermöglicht es uns, die Größe von Strings zu bestimmen und die richtige Logik für unsere Code zu implementieren.

## Wie geht das

Um die Länge eines Strings in Haskell zu finden, können wir die Funktion ```length``` verwenden. Diese Funktion nimmt einen String als Eingabe und gibt die Anzahl der Zeichen im String zurück. Hier ist ein Beispielcode, um die Länge des Strings "Hallo Welt" zu finden:

```Haskell
length "Hallo Welt"
```

Die Ausgabe dieses Codes wird ```10``` sein, da der String 10 Zeichen enthält, einschließlich Leerzeichen.

## Tiefer Einblick

Die Funktion ```length``` ist Teil der Standardbibliothek von Haskell, daher muss sie nicht importiert werden, um sie zu verwenden. Sie kann jedoch auch auf andere Arten implementiert werden, um die Länge eines Strings zu finden. Eine Möglichkeit besteht darin, eine rekursive Funktion zu schreiben, die jedes Zeichen im String zählt und eine Zählvariable erhöht, bis sie das Ende des Strings erreicht. Eine andere Möglichkeit besteht darin, eine Liste von Zeichen in einen String umzuwandeln und dann die Funktion ```length``` auf diese Liste anzuwenden. Weitere Informationen zu diesen Implementierungen findest du in der offiziellen Haskell-Dokumentation.

## Siehe auch

- [Haskell-Dokumentation zu length](https://www.haskell.org/hoogle/?hoogle=length)
- [Rekursion in Haskell](https://de.wikibooks.org/wiki/Haskell/kurs/Schleifen#Rekursion)
- [Strings in Haskell](https://wiki.haskell.org/String)