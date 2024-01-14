---
title:    "Haskell: Ein String in Großbuchstaben umwandeln."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist ein wichtiger Prozess in der Programmierung, der es uns ermöglicht, die Darstellung von Daten zu vereinheitlichen und sie leichter lesbar und verständlich zu machen. Es ist auch ein grundlegendes Konzept, das in vielen anderen Programmieraufgaben verwendet wird.

## Wie man

Um einen String in Haskell zu kapitalisieren, können wir die `toUpper` Funktion aus dem `Data.Char` Modul verwenden. Diese Funktion akzeptiert einen einzelnen Charakter als Argument und gibt den entsprechenden Charakter in Großbuchstaben zurück. Um diese Funktion auf einen gesamten String anzuwenden, können wir die `map` Funktion verwenden und sie auf jeden einzelnen Charakter im String anwenden.

Ein Beispielcode für die Kapitalisierung eines Strings in Haskell könnte wie folgt aussehen:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize str = map toUpper str
```

Wir können diesen Code in der GHCi-Interpreter ausführen, um die Ausgabe zu sehen:

```Haskell
> capitalize "hallo"
"HALLO"
```

Wir können auch eine Funktion schreiben, die ein einzelnes Wort in einem String kapitalisiert, anstatt den gesamten String. Dies kann mit der `words` Funktion aus dem `Data.List` Modul und der `unwords` Funktion aus demselben Modul erreicht werden. Das folgende Beispiel zeigt, wie das gemacht werden kann:

```Haskell
import Data.Char (toUpper)
import Data.List (words, unwords)

capitalizeWord :: String -> String
capitalizeWord str = unwords $ map capitalize (words str)
```

Dies ist ein Beispiel für den Einsatz von mehreren Funktionen und die Verkettung von Funktionen, um eine komplexere Aufgabe zu erledigen.

## Tiefergehende Informationen

In Haskell gibt es viele verschiedene Möglichkeiten, um Strings zu manipulieren, zu formatieren und zu verarbeiten. Das Kapitalisieren eines Strings ist nur eine davon. Es gibt auch Funktionen zum Konvertieren von Strings in kleinere Buchstaben, zum Entfernen von Leerzeichen oder anderen Zeichen aus einem String und vieles mehr.

Darüber hinaus gibt es auch verschiedene Bibliotheken und Pakete, die speziell für die Arbeit mit Strings entwickelt wurden, wie z.B. die `text` Bibliothek von Bryan O'Sullivan. Diese Bibliothek bietet leistungsstarke Funktionen für die Arbeit mit Text und Strings und ist eine wichtige Ressource für jeden, der in Haskell mit Strings arbeitet.

## Siehe auch

- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Data.Char Modul Dokumentation](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Data.List Modul Dokumentation](https://hackage.haskell.org/package/base/docs/Data-List.html)
- [Text Bibliothek von Bryan O'Sullivan](https://hackage.haskell.org/package/text)