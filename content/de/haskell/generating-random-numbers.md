---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Generierung von Zufallszahlen ist ein Prozess, bei dem jeder Ausgabe eine gleichmäßige Wahrscheinlichkeit eingeräumt wird. Programmierer erzeugen Zufallszahlen, um nichtdeterministischen Code zu schreiben, wie z. B. bei Spielen, Simulationen oder kryptografischen Anwendungen.

## So geht's:

Um eine Zufallszahl in Haskell zu erzeugen, verwenden wir das `randomRIO` Funktion aus dem `System.Random` Modul. Hier ist ein einfacher Code-Beispiel:

```Haskell
import System.Random    

main = do     
  zufallszahl <- randomRIO (1, 10) :: IO Int     
  print zufallszahl   
```

Wenn Sie es ausführen, gibt das Programm eine zufällige ganze Zahl zwischen 1 und 10 aus.

## Vertiefung

Das `System.Random` Modul in Haskell stammt aus den 1990er Jahren und hatte das Ziel, eine einfache und effiziente Methode zur Generierung von Zufallszahlen bereitzustellen. Alternativen zur Verwendung von `randomRIO` könnten die `random` und `randomR` Funktionen sein, die jedoch einen expliziten Zufallszahlengenerator benötigen. In Bezug auf die Implementierung verwendet Haskell den linearen Kongruenzgenerator als Zufallszahlengenerator.

## Siehe auch:

- Die `random` Bibliothek auf Hackage: [http://hackage.haskell.org/package/random](http://hackage.haskell.org/package/random)
- Der `random` Modul auf stackage: [https://www.stackage.org/lts-16.15/package/random-1.2.0](https://www.stackage.org/lts-16.15/package/random-1.2.0)