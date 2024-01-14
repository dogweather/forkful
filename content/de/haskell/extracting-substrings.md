---
title:    "Haskell: Unterstrings extrahieren"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung werden oft Zeichenketten (Strings) verarbeitet, die aus mehreren Wörtern oder Buchstaben bestehen. Manchmal möchten wir jedoch nur einen bestimmten Teil der Zeichenkette extrahieren und weiterverwenden. Dies ist der grundlegende Zweck einer Substring-Extraktion.

## Wie geht das?

Um Substrings in Haskell zu extrahieren, gibt es eine Funktion namens `take` und `drop`, die Teilstrings aus einer gegebenen Zeichenkette extrahiert. Die `take`-Funktion nimmt als Parameter die Anzahl der gewünschten Zeichen an und die `drop`-Funktion nimmt die Anzahl der am Anfang zu ignorierenden Zeichen an.

```Haskell
-- Beispiel für take
take 3 "Hallo Welt" -- gibt "Hal" zurück

-- Beispiel für drop
drop 5 "Hallo Welt" -- gibt " Welt" zurück
```

Zusätzlich gibt es noch die Funktion `substring`, die einen Startindex und eine Länge als Parameter nimmt.

```Haskell
-- Beispiel für substring
substring 3 5 "Hallo Welt" -- gibt "lo We" zurück
```

Für fortgeschrittene Anwendungen gibt es auch die Möglichkeit, reguläre Ausdrücke zu verwenden, um Substrings zu extrahieren. Hierfür steht die Funktion `=~` aus dem Paket `Text.Regex.Posix` zur Verfügung.

```Haskell
import Text.Regex.Posix -- Paket importieren

-- Beispiel für =~
"Hello World" =~ "[A-Z][a-z]+" :: String -- gibt "Hello" zurück
```

## Tiefergehende Informationen

Die Funktionen `take`, `drop` und `substring` sind Teil der Standardbibliothek von Haskell und können daher in jedem Programm verwendet werden. Sie sind jedoch nicht sehr flexibel und bieten keine Möglichkeit, zurückzugeben, welcher Teil der Zeichenkette tatsächlich extrahiert wurde.

Für diese anspruchsvolleren Anforderungen gibt es das Paket `text` mit der Funktion `splitAt`, die im Gegensatz zu `take` und `drop` sowohl den Teilstring als auch den Rest der ursprünglichen Zeichenkette zurückgibt.

```Haskell
import Data.Text -- Paket importieren

-- Beispiel für splitAt
splitAt 3 "Hallo Welt" -- gibt ("Hal", "lo Welt") zurück
```

Es ist auch möglich, mit Mustern zu arbeiten und Teilstrings basierend auf diesen zu extrahieren. Dafür ist das Paket `regex-base` mit der Funktion `splitRegex` geeignet.

```Haskell
import Text.Regex.Base -- Paket importieren

-- Beispiel für splitRegex
splitRegex (mkRegex "a+") "abcadefahij" -- gibt ["", "bc", "defhij"] zurück
```

## Siehe auch

- Offizielle Dokumentation über `substring`: https://www.haskell.org/hoogle/?hoogle=substring
- Paket `text` auf Hackage: https://hackage.haskell.org/package/text
- Paket `regex-base` auf Hackage: https://hackage.haskell.org/package/regex-base