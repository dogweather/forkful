---
title:    "Haskell: Umwandlung eines Zeichens in Kleinbuchstaben"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Strings in Kleinbuchstaben ist ein häufiges Problem in der Programmierung. Es kann hilfreich sein, wenn man zum Beispiel Daten vergleichen oder auf bestimmte Muster in Texten prüfen möchte. In dieser Blogpost werden wir uns anschauen, wie man String in Haskell in Kleinbuchstaben umwandelt.

## Wie es geht

Um einen String in Kleinbuchstaben zu konvertieren, können wir die Funktion `toLower` aus dem Modul `Data.Char` verwenden. Diese Funktion benötigt einen einzelnen `Char` als Argument und gibt den entsprechenden Kleinbuchstaben als `Char` zurück. Wenn wir diese Funktion auf jeden einzelnen Buchstaben in einem String anwenden, erhalten wir einen neuen String, in dem jeder Buchstabe in Kleinbuchstaben geschrieben ist.

```Haskell
import Data.Char

toLower 'A' -- gibt 'a' zurück
toLower 'H' -- gibt 'h' zurück
toLower '!' -- gibt '!' zurück (sonderzeichen bleiben unverändert)
```

Um einen vollständigen String in Kleinbuchstaben zu konvertieren, können wir die Funktion `map` verwenden, die eine Funktion auf jedes Element in einer Liste anwendet. In diesem Fall ist die Liste unser String, und die anzuwendende Funktion ist `toLower`.

```Haskell
map toLower "HALLO!" -- gibt "hallo!" zurück
```

## Tiefer Einblick

Um zu verstehen, wie die Funktion `toLower` in Haskell funktioniert, können wir uns den Quellcode anschauen. In der offiziellen Dokumentation von Haskell können wir sehen, dass die Funktion wie folgt definiert ist:

```Haskell
toLower :: Char -> Char
toLower c
  | 'A' <= c && c <= 'Z' = chr (ord c - ord 'A' + ord 'a')
  | otherwise = c
```

Die Funktion prüft zuerst, ob der übergebene `Char` ein Großbuchstabe ist. Wenn dies der Fall ist, wird mithilfe der Funktionen `chr` und `ord` der entsprechende Kleinbuchstabe bestimmt und zurückgegeben. Andernfalls bleibt der `Char` unverändert.

Diese Implementierung zeigt auch, dass Haskell eine starke Unterstützung für funktionale Programmierung bietet, da wir hier eine Funktion haben, die sich nicht auf veränderbare Variablen oder Seiteneffekte verlassen muss.

## Siehe auch

- [offizielle Haskell Dokumentation für `Data.Char`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)
- [Haskell Tutorial von LearnYouAHaskell.com](http://learnyouahaskell.com/chapters)