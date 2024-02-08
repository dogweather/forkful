---
title:                "Generierung von Zufallszahlen"
aliases:
- de/haskell/generating-random-numbers.md
date:                  2024-01-27T20:33:53.160311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in Haskell umfasst das Erstellen von Zahlen, die nach menschlichen Maßstäben unvorhersehbar sind. Dies ist kritisch in Szenarien, die von kryptografischen Anwendungen bis hin zu Simulationen reichen, bei denen das Element des Zufalls erforderlich ist, um reale Phänomene genau zu modellieren.

## Wie geht das:

Um Zufallszahlen in Haskell zu generieren, verwendet man typischerweise das `random` Paket, das Teil der Haskell Plattform ist. Hier ist eine Schritt-für-Schritt-Anleitung:

Zunächst stellen Sie sicher, dass Sie das `random` Paket installiert haben. Wenn nicht, können Sie es über Cabal oder Stack erhalten.

### Eine Zufallszahl generieren

Um eine einfache Zufallszahl zu generieren, können Sie die Funktion `randomRIO` verwenden, die einen zufälligen Wert innerhalb eines spezifizierten Bereichs erzeugt.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  zufallsZahl <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Zufallszahl: " ++ show zufallsZahl
```

### Eine Liste von Zufallszahlen generieren

Eine Liste von Zufallszahlen zu generieren ist etwas aufwändiger, aber immer noch unkompliziert:

```Haskell
import System.Random (randomRIO)

zufallsListe :: Int -> IO [Int]
zufallsListe 0 = return []
zufallsListe n = do
  r <- randomRIO (1, 100)
  rs <- zufallsListe (n-1)
  return (r:rs)

main :: IO ()
main = do
  zahlen <- zufallsListe 5
  print zahlen
```

Dieser Code-Schnipsel erstellt eine Funktion `zufallsListe`, die eine Liste von zufälligen Ganzzahlen generiert. Ersetzen Sie `(1, 100)` mit Ihrem gewünschten Bereich.

## Tiefergehend

Das Haskell `random` Paket stellt einen Pseudozufallszahlengenerator (PRNG) zur Verfügung, was bedeutet, dass die generierten Zahlen nicht wahrhaft zufällig sind, aber für viele Anwendungen als zufällig erscheinen können. Der Kern von Haskells Fähigkeit zur Zufallszahlengenerierung liegt in der `RandomGen` Typklasse, die verschiedene Methoden zur Generierung von Zufallszahlen abstrahiert, und der `Random` Typklasse, die Typen umfasst, die zufällig generiert werden können.

Historisch hat Haskell bei der Zufallszahlengenerierung Wert auf Reinheit und Reproduzierbarkeit gelegt. Das ist der Grund, warum Operationen, die Zufälligkeit involvieren, explizit in der `IO` Monad gehandhabt werden oder das manuelle Durchreichen und Aktualisieren von Generatorzuständen erfordern — um die Referenztransparenz zu erhalten.

In bestimmten Anwendungen, wie der Kryptografie, sind die vom Standard-PRNG generierten pseudozufälligen Zahlen möglicherweise nicht sicher genug. Für diese Anwendungsfälle wenden sich Haskell-Programmierer oft an spezialisiertere Bibliotheken wie `crypto-random`, die darauf ausgelegt sind, die strengen Anforderungen kryptografischer Anwendungen zu erfüllen.

Darüber hinaus bieten alternative Bibliotheken wie `mwc-random` bessere Leistung und Qualität der Zufallszahlen für Simulationen und andere Anwendungen, indem sie moderne Algorithmen wie den Mersenne-Twister implementieren.

Bei der Auswahl eines Ansatzes zur Zufallszahlengenerierung in Haskell ist es wesentlich, die Bedürfnisse der Anwendung in Bezug auf Qualität der Zufälligkeit, Leistung und Sicherheit zu berücksichtigen, um das am besten geeignete Werkzeug oder Bibliothek auszuwählen.
