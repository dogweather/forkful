---
title:                "Erzeugen von zufälligen Zahlen"
html_title:           "Haskell: Erzeugen von zufälligen Zahlen"
simple_title:         "Erzeugen von zufälligen Zahlen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen sind eine wichtige Komponente in der Programmierung. Sie werden verwendet, um uns vorhersehbare Ergebnisse zu liefern, die uns helfen, Entscheidungen zu treffen oder Simulationen durchzuführen.

Das Generieren von Zufallszahlen ist entscheidend, um bestimmte Probleme zu lösen. Durch die Verwendung von Zufallszahlen können wir komplexe Probleme angehen, wie zum Beispiel die Erzeugung von Verschlüsselungsschlüsseln oder die Erstellung von zufälligen Testdaten für unsere Programme.

## Wie geht es?

Das Generieren von Zufallszahlen in Haskell ist einfach und unkompliziert. Wir können die Funktion `randomR` aus dem Modul `System.Random` verwenden, um eine Zufallszahl im angegebenen Bereich zu generieren. Hier ist ein Beispiel:

```Haskell
import System.Random

randomNumber :: IO Int
randomNumber = randomR (1, 100::Int)

main :: IO ()
main = do
  num <- randomNumber
  print num
```

Die Funktion `randomNumber` gibt eine Zufallszahl zwischen 1 und 100 aus. Wir können dies in der `main`-Funktion ausgeben und das Ergebnis wird jedes Mal unterschiedlich sein.

Output:

`75`

## Tiefer Graben

Das Generieren von Zufallszahlen ist ein komplexes Thema und hat in der Geschichte der Informatik eine wichtige Rolle gespielt. Früher wurde es verwendet, um Simulationen von zufälligen Ereignissen zu erstellen. Heutzutage wird es für verschiedene Aufgaben wie die Erstellung von Passwörtern oder die Erzeugung von Schlüsseln für Kryptographie verwendet.

Es gibt auch alternative Methoden, um Zufallszahlen in Haskell zu generieren. Zum Beispiel können wir die `random`-Funktion verwenden, um eine Zufallszahl zwischen 0 und 1 zu generieren. Dies kann nützlich sein, wenn wir eine Wahrscheinlichkeitsverteilung für unsere Zufallszahlen berechnen möchten.

In Haskell werden Zufallszahlen nicht tatsächlich zufällig generiert. Stattdessen werden deterministische Algorithmen verwendet, die auf einem Seed-Wert basieren. Dieser Seed-Wert kann angegeben oder automatisch generiert werden und beeinflusst die generierten Zufallszahlen.

## Siehe auch

- [Haskell-Dokumentation zu Zufallsgeneratoren](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Cheat sheet für die Verwendung von Zufallszahlen in Haskell](https://www.fpcomplete.com/haskell/tutorial/random-in-haskell)
- [HaskellWiki über Zufallszahlen](https://wiki.haskell.org/Random_number_generation)