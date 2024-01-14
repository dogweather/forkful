---
title:                "Haskell: Zufallszahlen generieren"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung gibt es oft Situationen, in denen man zufällige Zahlen benötigt. Das können simulierte Spiele oder statistische Analysen sein. Mit Haskell kann man auf einfache Weise zufällige Zahlen generieren und somit ein vielseitiges Werkzeug für verschiedene Anwendungen schaffen.

## Wie es geht

Die Haskell-Funktion `randomR` ermöglicht es uns, eine zufällige Zahl innerhalb eines bestimmten Bereichs zu erzeugen. Wir können dies nutzen, um beispielsweise eine zufällige Zahl zwischen 1 und 10 zu generieren:

```Haskell
randomR (1,10) :: RandomGen g => g -> (Int, g)
```

Hier sehen wir, dass `randomR` ein Typklasse `RandomGen` erfordert und einen Tupel von Integer und Generatoren zurückgibt. Wir können diese Funktion mit einer Variablen von Typ `StdGen` verwenden, die eine Startzahl für unsere Zufallszahlengenerierung enthält:

```Haskell
gen <- getStdGen
let (number, newGen) = randomR (1,10) gen
```

Der Wert `number` enthält nun eine zufällige Zahl zwischen 1 und 10 und `newGen` ist eine aktualisierte Version unseres Generators, die für die nächste Zufallszahl verwendet werden kann.

## Tieferer Einblick

Das Konzept der Zufallszahlengenerierung in Haskell basiert auf einer sogenannten "reinen" Funktion, die für dieselben Eingabewerte immer denselben Ausgabewert liefert. Um dieses Konzept auf Zufallszahlen anzuwenden, werden Generatoren verwendet, die bei jedem Aufruf eine neue Zahl erzeugen, aber dennoch deterministisch sind. Das bedeutet, dass dieselben Eingabewerte immer dieselben zufälligen Zahlen erzeugen werden.

Um die `randomR` Funktion noch besser zu verstehen, können wir uns die zugrunde liegenden Generatoren und Algorithmen genauer ansehen. In Haskell gibt es verschiedene Generatoren mit unterschiedlichen Eigenschaften, die je nach Anwendung ausgewählt werden können.

## Siehe auch

- [Die offizielle Dokumentation zu `randomR`](https://hackage.haskell.org/package/random/docs/System-Random.html#v:randomR)
- [Eine kurze Einführung in Haskell](https://haskell.org) (Englisch)
- [Weitere Artikel zu fortgeschrittenen Themen in Haskell](https://wiki.haskell.org/Research_papers/Functional_pearls) (Englisch)