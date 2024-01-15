---
title:                "Ausgabe von Debugging-Informationen"
html_title:           "Haskell: Ausgabe von Debugging-Informationen"
simple_title:         "Ausgabe von Debugging-Informationen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Du bist gerade dabei, dein Haskell-Programm zu entwickeln und stößt auf unerklärliche Fehler? Das Drucken von Debug-Ausgaben kann dir dabei helfen, deine Code zu verstehen und Probleme zu lösen. In diesem Artikel werden wir dir zeigen, wie du Debug-Ausgaben in Haskell nutzen kannst.

## Wie funktioniert es

Um Debug-Ausgaben in Haskell zu drucken, verwenden wir die Funktion `print`, die einen Wert auf die Standardausgabe ausgibt. Hier ist ein Beispiel, wie wir eine Debug-Ausgabe in unserem Code platzieren können:

```Haskell
foo :: Int -> Int -> Int
foo x y = x + y

main = do
    let result = foo 2 3
    print result
```

In diesem Beispiel haben wir die Funktion `foo` definiert, die zwei Integers addiert. In der `main` Funktion haben wir `foo` mit den Argumenten 2 und 3 aufgerufen und das Ergebnis daraus in der Variable `result` gespeichert. Anschließend drucken wir `result` mit der `print` Funktion aus. Wenn wir nun unser Programm ausführen, sollten wir `5` in unserer Konsole sehen.

Dies ist nur ein einfaches Beispiel, aber du kannst Debug-Ausgaben in jedem Teil deines Codes platzieren, um den Wert von Variablen oder Ausdrücken zu überprüfen.

## Der tiefe Tauchgang

Es gibt eine weitere nützliche Funktion namens `trace`, die es uns ermöglicht, Debug-Ausgaben mit zusätzlichen Informationen zu versehen. Hier ist ein Beispiel:

```Haskell
import Debug.Trace (trace)

foo :: Int -> Int -> Int
foo x y = x + y

main = do
    let result = foo 2 3
    trace ("Das Ergebnis von foo ist " ++ (show result)) result
```

Wenn wir nun unser Programm ausführen, sehen wir nicht nur `5`, sondern auch unsere zusätzliche Information in der Konsole: "Das Ergebnis von foo ist 5". Dies kann besonders hilfreich sein, wenn du versuchst, die Reihenfolge oder den Wert verschiedener Ausdrücke in deinem Code zu überprüfen.

## Siehe auch

- [Haskell Debugging](https://wiki.haskell.org/Debugging)
- [Learn You a Haskell - Debugging](http://learnyouahaskell.com/starting-out#debugging) 
- [Haskell Documentation - Debugging Tools](https://www.haskell.org/documentation/#debugging-tools)