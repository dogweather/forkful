---
title:                "Haskell: Großschreibung eines Strings"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung müssen wir oft mit Eingaben arbeiten, die nicht immer in der gewünschten Form sind. Eine häufige Aufgabe besteht darin, einen Text oder eine Zeichenfolge zu formatieren, indem man den ersten Buchstaben jedes Wortes groß schreibt. Das kann aus ästhetischen oder funktionalen Gründen notwendig sein. Das Kapitalisieren einer Zeichenfolge ist eine grundlegende Aufgabe, die in vielen Anwendungen nützlich sein kann.

## Wie geht das?

Um eine Zeichenfolge in Haskell zu kapitalisieren, müssen wir zuerst die `toUpper` Funktion aus dem`Data.Char` Modul importieren. Dann können wir die `map` Funktion verwenden, um die `toUpper` Funktion auf jede Zeichenfolge in unserer Eingabe anzuwenden. Schließlich kombinieren wir alles mit der `unwords` Funktion, um die einzelnen Wörter wieder zusammenzufügen.

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = unwords . map (toUpper . head) . words
```

Die `capitalize` Funktion nimmt eine Zeichenfolge als Eingabe und gibt eine neue Zeichenfolge zurück, in der der erste Buchstabe jedes Wortes groß geschrieben ist.

```Haskell
capitalize "das ist ein Test" -- Output: "Das Ist Ein Test"
```

## Tiefergehender Einblick

Die Hauptfunktion, die hier verwendet wird, ist die `map` Funktion, die eine andere Funktion auf jedes Element einer Liste anwendet und eine neue Liste zurückgibt. In unserem Fall ist das Element eine Zeichenfolge und die Funktion ist die `toUpper` Funktion, die einen Buchstaben in Großbuchstaben umwandelt. Dann machen wir von den `words` und `unwords` Funktionen Gebrauch, um die Eingabe in eine Liste von Wörtern zu zerlegen und sie wieder zusammenzufügen.

Es gibt auch andere Möglichkeiten, eine Zeichenfolge zu kapitalisieren, zum Beispiel mit der `head` und `tail` Funktionen, die den ersten bzw. alle außer dem ersten Buchstaben einer Zeichenfolge zurückgeben. Sie können auch reguläre Ausdrücke verwenden oder sogar eine eigene Funktion schreiben, um die Großschreibung zu implementieren. Es gibt viele verschiedene Wege, um dieselbe Aufgabe zu lösen, und es lohnt sich, verschiedene Ansätze auszuprobieren und herauszufinden, was für Ihre Situation am besten geeignet ist.

## Siehe auch

- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [Haskell für Praktiker](https://haskell-from-kentikelenis.netlify.app/) (auf Deutsch)