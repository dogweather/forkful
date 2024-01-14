---
title:    "Elm: Zufällige Zahlen generieren"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es oft die Notwendigkeit, zufällige Zahlen zu generieren. Zum Beispiel können sie in Spielen für verschiedene Ereignisse verwendet werden, oder um ein Element aus einer Liste auszuwählen. In Elm wird die Generierung von Zufallszahlen durch das Modul `Random` ermöglicht. In diesem Artikel werden wir uns genauer anschauen, wie man zufällige Zahlen in Elm generieren kann.

# Wie geht das?

Um das Modul `Random` nutzen zu können, müssen wir es zuerst importieren: 

```Elm
import Random
```

Als nächstes müssen wir einen generischen Typ für die zu generierende Zufallszahl angeben, zum Beispiel `Int` für ganze Zahlen oder `Float` für Gleitkommazahlen. Dann können wir eine `Seed` (Samen) angeben, die für die Generierung der Zufallszahlen verwendet wird. Diese können wir mithilfe von `Random.initialSeed` generieren.

Hier ist ein Beispiel, wie man eine zufällige Ganzzahl zwischen 1 und 10 generieren kann: 

```Elm
Random.generate randomNumber (Random.int 1 10)
    -- Seed: Random.initialSeed
randomNumber int =
    Debug.log (toString int) int
```

Dieses Beispiel nutzt die Funktion `Random.generate`, um die zufällige Zahl zu generieren. Diese Funktion erwartet zwei Argumente: eine Funktion, die das Ergebnis des Zufallsgenerators verwaltet, und den eigentlichen Zufallsgenerator (in diesem Fall `Random.int 1 10`).

Um das Ergebnis zu erhalten, nutzen wir die Funktion `Debug.log` und geben die generierte Zahl zusammen mit dem generierten Seed als Argumente an.

# Deep Dive

`Random` bietet viele verschiedene Funktionen und Möglichkeiten für die Generierung von Zufallszahlen. Zum Beispiel kann man mithilfe von `Random.generate` auch mehrere Zufallszahlen auf einmal generieren. Auch können verschiedene Wahrscheinlichkeitsverteilungen für die Generierung von Zufallszahlen ausgewählt werden.

Ein wichtiger Punkt bei der Generierung von Zufallszahlen ist die `Seed`. Der Seed bestimmt, welche Zahlen generiert werden und in welcher Reihenfolge. Wenn man denselben Seed verwendet, bekommt man auch dieselben Ergebnisse. Um verschiedene Ergebnisse zu erhalten, kann man den Seed zufällig generieren oder manuell ändern.

# Siehe auch

- Offizielle Dokumentation von Elm zu `Random`: https://elm-lang.org/docs/random
- Eine Einführung in die Generierung von Zufallszahlen in Elm: https://blog.echobind.com/random-number-generation-in-elm-864945933d1d
- Eine vertiefte Analyse von `Random` und seiner Verwendung in Elm: https://guide.elm-lang.org/effects/random.html