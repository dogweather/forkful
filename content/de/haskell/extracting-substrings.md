---
title:                "Haskell: Extrahieren von Unterstrings"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Die Extraktion von Teilstrings ist eine nützliche Fähigkeit für jeden Programmierer, der mit Texten arbeitet. Mit dieser Technik können wir schnell und präzise bestimmte Teilbereiche von Zeichenketten extrahieren und weiterverarbeiten. In diesem Blog-Post werden wir uns ansehen, wie man diese Aufgabe in Haskell lösen kann.

## Wie geht man vor

Das `Data.Text` Modul bietet uns einige nützliche Funktionen für die Manipulation von Texten. Eine davon ist die `take` Funktion, die es uns ermöglicht, eine bestimmte Anzahl von Zeichen aus einem Text zu extrahieren. Schauen wir uns zunächst ein Beispiel an:

```Haskell
import Data.Text

myText = pack "Dies ist ein Beispieltext."
extractedText = take 4 myText
```

Bei der Kompilierung sehen wir nun, dass der Wert für `extractedText` "Dies" lautet. Wir haben also erfolgreich die ersten 4 Zeichen aus unserem Text extrahiert. Aber was ist, wenn wir nicht nur die ersten 4 Zeichen, sondern die ersten 6 extrahieren wollen? Hier kommt die `take` Funktion ins Spiel, die uns die Länge des gewünschten Teiltexts angibt.

Wie können wir jedoch die Zeichen an einem bestimmten Index extrahieren? Hier kommt die `index` Funktion ins Spiel. Schauen wir uns ein Beispiel an:

```Haskell
myText = pack "Dies ist ein Beispieltext."
extractedChar = index myText 5
```

In diesem Fall würden wir das Zeichen "i" extrahieren, das an der 5. Stelle des Textes steht. Mit diesen zwei Funktionen haben wir bereits eine solide Grundlage, um Teilstrings zu extrahieren.

## Tiefere Einblicke

Die `take` und `index` Funktionen sind sehr nützlich, aber wir können noch weiter in die Tiefe gehen. Wenn wir zum Beispiel jemanden haben, der unseren Text überprüfen und mitteilen soll, ob ein bestimmter Teiltext darin enthalten ist, können wir die `isInfixOf` Funktion verwendet werden. Schauen wir uns ein Beispiel an:

```Haskell
myText = pack "Dies ist ein Beispieltext."
checkText = "ei"
result = isInfixOf checkText myText
```

Bei der Kompilierung sehen wir, dass der Wert für `result` `True` lautet, da der Teiltext "ei" im Text enthalten ist.

Das `Data.Text` Modul bietet uns auch die `breakOn` Funktion, mit der wir eine Zeichenkette in zwei Teilstrings aufteilen können. Schauen wir uns ein Beispiel an:

```Haskell
myText = pack "Dies ist ein Beispieltext."
slicedText = breakOn "ist" myText
```

Wir erhalten nun als Ergebnis ein Tupel, in dem der erste Teilstring "Dies " und der zweite Teilstring "ist ein Beispieltext." sind.

## Siehe auch

- [Haskell-Dokumentation zum Data.Text-Modul] (https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [Haskell-Grundlagen für Anfänger] (https://www.haskell.org/tutorial/)