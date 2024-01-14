---
title:    "Haskell: Zufällige Zahlen generieren"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung, da es ermöglicht, unberechenbare und vielseitige Ergebnisse zu erzeugen. Es kann für Simulationen, Spiele, Verschlüsselung und vieles mehr verwendet werden.

# Wie man Zufallszahlen in Haskell generiert

Das Generieren von Zufallszahlen in Haskell ist einfach und effektiv. Zunächst müssen wir das `System.Random` Modul importieren. Dann können wir die `Random`-Funktion verwenden, um eine Zufallszahl innerhalb eines bestimmten Bereichs zu generieren. Zum Beispiel:

```Haskell
import System.Random

-- Generiert eine Zufallszahl zwischen 1 und 10
randomNum :: IO Int
randomNum = randomRIO (1,10)

-- Generiert eine Zufallszahl zwischen 0 und 1
randomFloat :: IO Float
randomFloat = randomRIO (0,1)
```

Die `randomRIO` Funktion nimmt ein Tupel als Argument und gibt eine Zufallszahl in diesem Bereich zurück. Wir können auch `randomIO` verwenden, um eine Zufallszahl mit einem beliebigen Datentyp zu erzeugen.

# Tieferer Einblick

Beim Generieren von Zufallszahlen ist es wichtig, dass sie wirklich zufällig sind. Um dies zu gewährleisten, basiert die Zufallszahlen-Generierung in Haskell auf einem Zustand. Das bedeutet, dass jede generierte Zufallszahl von dem zuvor generierten Zustand abhängt.

Um eine deterministische Zufallszahl, also eine, die immer dieselbe ist, zu erzeugen, müssen wir den Zustand manuell festlegen. Dazu können wir die `mkStdGen` Funktion verwenden. Diese Funktion nimmt eine beliebige Nummer als Argument und generiert einen Zustand, der an die `randoms` Funktion übergeben werden kann, um eine deterministische Zufallszahlsequenz zu erhalten.

# Sieh auch

- [Haskell Dokumentation zu Zufallszahlen](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Gutes Tutorial zum Generieren von Zufallszahlen in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms)
- [Haskell Codebeispiele zum Generieren von Zufallszahlen](http://learnyouahaskell.com/input-and-output#randomness)