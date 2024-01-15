---
title:                "Erzeugung von zufälligen Zahlen"
html_title:           "Haskell: Erzeugung von zufälligen Zahlen"
simple_title:         "Erzeugung von zufälligen Zahlen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Generierung von Zufallszahlen befassen? Nun, Zufallszahlen sind ein wichtiger Bestandteil vieler Anwendungen, wie z.B. Simulationen, Spiele oder kryptographische Algorithmen. Sie ermöglichen es uns, unberechenbare und unvorhersehbare Daten zu generieren, die für verschiedene Zwecke nützlich sein können.

## Wie man Zufallszahlen in Haskell generiert

Um Zufallszahlen in Haskell zu generieren, müssen wir zuerst das "random" Modul importieren, das Teil der standardmäßigen Bibliothek ist. Dann können wir die Funktion "randomRIO" verwenden, um eine Zufallszahl innerhalb eines bestimmten Bereichs zu generieren. Hier ist ein Beispielcode, der eine Zufallszahl zwischen 1 und 10 erzeugt:

```Haskell
import System.Random

main = do
  randomNum <- randomRIO (1,10)
  putStrLn $ "Die Zufallszahl ist: " ++ show randomNum
```

Die "randomRIO" Funktion gibt eine Zufallszahl vom Typ "IO Int" zurück, daher müssen wir sie mit der Bindung " <- " ausführen und mit der Funktion "putStrLn" ausgeben.

## Tiefer Einblick

Die "randomRIO" Funktion verwendet das "RandomGen" Typklasse, um einen Zufallsgenerator zu erstellen. Standardmäßig verwendet Haskell den "StdGen" Zufallsgenerator, der auf der Zeit basiert. Dies bedeutet, dass jede Ausführung des Codes eine andere Zufallszahl erzeugt. Wir können jedoch auch unseren eigenen Zufallsgenerator erstellen, indem wir die "mkStdGen" Funktion verwenden und einen beliebigen Startwert übergeben.

Es ist auch möglich, komplexe Datenstrukturen mit zufälligen Werten zu erstellen, indem wir die Funktion "random" verwenden, die in der "Random" Typklasse enthalten ist. Diese Funktion hat einen generischen Typ, der es uns ermöglicht, eine Zufallszahl aus jeder beliebigen Typklasse zu generieren, die eine Instanz von "Random" ist.

## Siehe auch

- [Offizielle Dokumentation über das "random" Modul](https://hackage.haskell.org/package/random)
- [Eine Einführung in Haskell mit zufälligen Zahlen als Beispiel](https://www.haskell.org/tutorial/randomness.html)
- [Ein praktisches Tutorial zur Verwendung von Zufallszahlen in Haskell](https://www.fpcomplete.com/blog/2017/01/randomness-haskell)