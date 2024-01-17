---
title:                "Die Länge eines Strings finden"
html_title:           "Haskell: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Finden der Länge eines Strings ist eine häufige Aufgabe, die Programmierer in vielen Programmiersprachen lösen müssen. Die Länge eines Strings bezieht sich auf die Anzahl der Zeichen in einem Text oder einer Zeichenkette. Dies ist wichtig, um bestimmte Manipulationen an einem String durchzuführen oder um zu überprüfen, ob er eine bestimmte Länge hat.

## Wie geht das?

Die Länge eines Strings kann in Haskell auf verschiedene Arten gefunden werden. Eine Möglichkeit besteht darin, die Funktion ```length``` zu verwenden, die in der standardmäßigen Modulbibliothek ```Data.List``` verfügbar ist. Dies ermöglicht es uns, schnell die Länge eines Strings zu finden, indem wir einfach ```length``` gefolgt von dem betreffenden String schreiben.

Beispielcode:

```Haskell
import Data.List

length "Hallo Welt" -- gibt 11 aus, da der String 11 Zeichen hat.
```

Eine andere Möglichkeit besteht darin, die Rekursion zu nutzen, indem wir jedes Zeichen in der Zeichenkette zählen und dann die Anzahl zurückgeben. Dies ermöglicht es uns, die Länge eines Strings unabhängig von einer vorgefertigten Funktion zu finden.

Beispielcode:

```Haskell
length' :: String -> Int -- Funktion, die eine Zeichenkette als Argument erwartet und eine Ganzzahl zurückgibt.
length' "" = 0 -- Basisfall, leere Zeichenkette hat Länge 0.
length' (x:xs) = 1 + length' xs -- Zählt jedes Zeichen und gibt die Länge zurück, indem die Funktion für den Rest der Zeichenkette aufgerufen wird.

length' "Hallo Welt" -- gibt 11 aus, wie auch die vorherige Methode.
```
## Tiefergehende Infos

Das Ermitteln der Länge eines Strings ist eine grundlegende Funktion in vielen Programmiersprachen und hat ihren Ursprung in der Theorie der formalen Sprachen und Automaten. Es gibt auch alternative Möglichkeiten, um die Länge eines Strings in Haskell zu finden, wie zum Beispiel die Verwendung der Bibliothek ```Data.Text```. Diese bietet schnellere Operationen auf Texten mit einer größeren Anzahl an Zeichen an. 

In Bezug auf die Implementierungsdetails nutzt die Funktion ```length``` in ```Data.List``` einfache Iteration, um die Länge zu finden, während die rekursive Funktion ```length'``` Zeichen zählt und dann die Anzahl zurückgibt. Es ist wichtig zu beachten, dass die rekursive Funktion möglicherweise schneller ist als die iterative Variante, da sie keine interne Schleife durchläuft.

## Siehe auch

- Offizielle Dokumentation für ```Data.List```: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html
- Offizielle Dokumentation für ```Data.Text```: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html