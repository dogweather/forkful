---
title:    "Haskell: Zufallszahlen generieren"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiges Konzept in der Programmierung. Es ermöglicht uns, zufällige Ergebnisse in unsere Programme einzubauen, was besonders in der Spieleentwicklung und Simulationen nützlich ist.

## Wie man es macht

In Haskell gibt es verschiedene Bibliotheken und Techniken, mit denen wir Zufallszahlen generieren können. Eine beliebte Option ist die `random` Bibliothek, die Funktionen zur Erzeugung von Zufallszahlen verschiedener Datentypen wie Integer und Gleitkommazahlen bereitstellt. Schauen wir uns ein Beispiel an:

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let (r, _) = randomR (1, 100) gen -- generiert eine Zufallszahl zwischen 1 und 100
  print ("Deine zufällige Zahl ist: " ++ show r)
```

Dieses kleine Programm nutzt die `getStdGen` Funktion, um einen Zufallsgenerator zu initialisieren, und gibt dann eine zufällige Zahl zwischen 1 und 100 aus.

Aber was ist, wenn wir mehrere Zufallszahlen auf einmal generieren möchten? Hier kommt die `randoms` Funktion ins Spiel, die uns eine unendliche Liste von Zufallszahlen zurückgibt. Schauen wir uns ein Beispiel an:

```Haskell
import System.Random

main = do
  gen <- getStdGen
  let rands = take 10 $ randoms gen :: [Int] -- generiert eine Liste von 10 Zufallszahlen
  print ("Deine Liste von Zufallszahlen ist: " ++ show rands)
```

## Tiefergehende Informationen

Es gibt noch viele weitere Nuancen und Techniken beim Generieren von Zufallszahlen in Haskell. Beispielsweise gibt es auch die Möglichkeit, nicht-deterministische Funktionen zu schreiben, die bei jedem Aufruf unterschiedliche Ergebnisse liefern. Eine tiefergehende Beschäftigung mit diesem Thema kann dabei helfen, ein besseres Verständnis von Funktionsprogrammierung zu bekommen.

## Siehe auch

- [Haskell-Dokumentation zur `random` Bibliothek](https://hackage.haskell.org/package/random)
- [Haskell-Dokumentation zur `System.Random` Bibliothek](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html)
- [Video-Tutorial zur Verwendung von Zufallszahlen in Haskell](https://www.youtube.com/watch?v=dz3LWfyaAs8)