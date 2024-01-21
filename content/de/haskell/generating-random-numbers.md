---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:16.287955-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erzeugen von Zufallszahlen, oder Randomisierung, ist ein Prozess, bei dem durch eine Berechnung nicht vorhersagbare Zahlen produziert werden. Programmierer nutzen dies für Spiele, Simulationen, Tests oder überall dort, wo Ergebnisse variieren sollen.

## Wie geht das:
Die Haskell-Standardbibliothek bietet Funktionen für Zufallszahlen. Hier ist ein einfaches Beispiel:

```Haskell
import System.Random (randomRIO)

-- Generiert eine Zufallszahl zwischen 1 und 10
erzeugeZufallszahl :: IO Int
erzeugeZufallszahl = randomRIO (1, 10)

main :: IO ()
main = do
    zahl <- erzeugeZufallszahl
    putStrLn $ "Deine Zufallszahl ist: " ++ show zahl
```

Ausführung und mögliche Ausgabe:

```
Deine Zufallszahl ist: 7
```

## Deep Dive
Zufallszahlen in Haskell werden oft mithilfe des `random`-Moduls erzeugt, das vom Typklassenansatz Gebrauch macht. Historisch gesehen haben unterschiedliche Sprachen verschiedene Ansätze zur Randomisierung verfolgt, Haskell vermag jedoch mit Typsicherheit und funktionaler Reinheit zu punkten. Alternativ zur standard `IO`-basierten Funktion `randomRIO` gibt es `randomR` im `Random`-Modul, das rein ist und mit einem expliziten Seed arbeitet. Es erweitert die Möglichkeiten für den Entwickler, fordert jedoch eine genauere Behandlung des Zustands.

## Siehe auch
- Haskell `random` Modul Dokumentation: https://hackage.haskell.org/package/random
- Wiki über Pseudozufallszahlengeneratoren: https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Eine Diskussion über Zufallszahlen in Haskell auf StackOverflow: https://stackoverflow.com/questions/tagged/random+haskell