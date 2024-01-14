---
title:                "Haskell: Erzeugen von Zufallszahlen"
simple_title:         "Erzeugen von Zufallszahlen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Random Numbers, also zufällig generierte Zahlen, sind in vielen Bereichen der Informatik von großer Bedeutung. Sie werden zum Beispiel bei Simulationen, Verschlüsselungen oder Spielen verwendet. Auch in der funktionalen Programmiersprache Haskell können wir Random Numbers nutzen, um unsere Programme interessanter und dynamischer zu gestalten.

## Wie man Random Numbers generiert

Um in Haskell Random Numbers zu generieren, müssen wir zunächst das "System.Random" Modul importieren. Dies ermöglicht uns den Zugriff auf die Funktionalitäten zur Erzeugung von zufälligen Zahlen.

Um eine einzelne zufällige Zahl zu erzeugen, können wir die Funktion "randomRIO" verwenden. Diese Funktion nimmt zwei Parameter entgegen, ein Mindest- und ein Maximalwert, und gibt uns eine zufällige Zahl innerhalb dieses Bereichs zurück. Hier ist ein Beispielcode:

```Haskell
import System.Random

main = do
    randomNumber <- randomRIO (1, 10)
    putStrLn ("Die zufällige Zahl ist: " ++ show randomNumber)
```

Wenn wir dieses Programm ausführen, erhalten wir jedes Mal eine andere zufällige Zahl zwischen 1 und 10.

Um eine Liste von zufälligen Zahlen zu generieren, können wir die Funktion "randomRs" nutzen. Diese Funktion nimmt ebenfalls einen Bereich als Parameter entgegen, gibt uns aber eine unendliche Liste von zufälligen Zahlen zurück. Hier ist ein Beispielcode:

```Haskell
import System.Random

main = do
    randomNumbers <- take 10 <$> randomRs (1, 100)
    putStrLn ("Die Liste der zufälligen Zahlen ist: " ++ show randomNumbers)
```

In diesem Beispiel nutzen wir die "take" Funktion, um nur die ersten 10 Elemente der unendlichen Liste zu erhalten. Wir können auch andere Funktionen wie "cycle" oder "filter" anwenden, um interessante Verwendungen der zufälligen Zahlen zu entdecken.

## Tieferer Einblick

Das "System.Random" Modul bietet uns noch weitere Funktionen und Möglichkeiten, um Random Numbers zu nutzen. Zum Beispiel können wir auch komplexere Datentypen wie Zufallslisten oder zufällige Permutationen von Listen generieren.

Als Alternative zum "System.Random" Modul können wir auch das "random" Paket nutzen, welches zusätzliche Funktionen zur Verfügung stellt, wie zum Beispiel die Erzeugung von Zufallszahlen mit bestimmten Verteilungen oder das Generieren von Zufallszahlen in Kombination mit Monaden.

## Siehe auch

- https://hackage.haskell.org/package/random
- https://wiki.haskell.org/Random
- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/randoms