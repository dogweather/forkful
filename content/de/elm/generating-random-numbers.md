---
title:                "Elm: Erzeugen von Zufallszahlen"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

Das Generieren von Zufallszahlen spielt eine wichtige Rolle bei der Entwicklung von Software. Zum Beispiel kann es in Spielen verwendet werden, um zufällige Ereignisse zu simulieren oder in der Kryptographie, um Schlüssel zu generieren. In dieser Blog-Post werden wir uns mit der Generierung von Zufallszahlen in der funktionalen Programmiersprache Elm beschäftigen und wie man sie in Projekten verwenden kann.

## Wie es geht

Um Zufallszahlen in Elm zu generieren, verwenden wir die `Random` Bibliothek. Zunächst müssen wir sie importieren, indem wir am Anfang unseres Codes `import Random` schreiben.

Dann können wir `Random.generate` verwenden, um eine Zufallszahl zu generieren. Zum Beispiel können wir eine Funktion schreiben, die eine Zufallszahl zwischen 1 und 10 generiert und sie in der Konsole ausgibt:

```Elm
import Random 

randomNumber : Int
randomNumber =
  Random.generate (\_ -> Random.int 1 10) 

main =
  randomNumber |> Debug.log "Random Number"
```

In diesem Beispiel wird die `Random.int` Funktion verwendet, um eine Zufallszahl zwischen 1 und 10 zu generieren. Wir verwenden das `_` Argument in der `Random.generate` Funktion, da wir es nicht verwenden und es einfach überspringen können.

Unsere Ausgabe sieht dann folgendermaßen aus:

```
Random Number: 5
```

Um mehrere Zufallszahlen in einer Liste zu generieren, können wir `List.map` und `Random.generate` zusammen verwenden. Zum Beispiel können wir eine Funktion schreiben, die eine Liste mit 5 zufälligen Zahlen zwischen 1 und 10 generiert:

```Elm
import Random 
import List

randomNumberList : List Int
randomNumberList =
  List.map (\_ -> Random.int 1 10) (List.range 1 5) 

main =
  randomNumberList |> Debug.log "Random Number List"
```

In diesem Beispiel wird die `List.map` Funktion verwendet, um die `Random.generate` Funktion 5 Mal auszuführen und eine Liste mit den Ergebnissen zurückzugeben. Unsere Ausgabe sieht dann folgendermaßen aus:

```
Random Number List: [3, 8, 2, 9, 7]
```

Es gibt auch andere Funktionen in der `Random` Bibliothek, die wir verwenden können, um verschiedene Arten von Zufallszahlen zu generieren, wie zum Beispiel Gleitkommazahlen oder Booleans. Weitere Informationen dazu findest du in der "## Deep Dive" Sektion.

## Deep Dive

In der `Random` Bibliothek gibt es einige Funktionen, die verschiedene Aspekte der Generierung von Zufallszahlen kontrollieren. Beispielsweise können wir mit der `Random.step` Funktion die Seed-Nummer für die Zufallszahlengenerierung festlegen, um wiederholbare Ergebnisse zu erhalten. Wir können auch mit der `Random.initialSeed` Funktion eine Seed-Nummer aus dem Systemzeitstempel generieren lassen.

Es ist auch möglich, eigene Generatoren für Zufallszahlen zu erstellen, indem man die `Random.Generator` Funktion verwendet. Dies ermöglicht es uns, benutzerdefinierte Logik für die Generierung von Zufallszahlen zu schreiben.

Weitere Details zu diesen Funktionen findest du in der offiziellen Elm Dokumentation zur [Random Bibliothek](https://package.elm-lang.org/packages/elm/random/latest/Random).

## Siehe auch
- Offizielle Elm Dokumentation zur [Random Bibliothek](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [An Introduction to Random Numbers in Elm](https://medium.com/@noahprusik/an-introduction-to-random-numbers-in-elm-8e1866ca97d5)
- [Exploring Random Generation in Elm](https://www.valentinog.com/blog/exploring-elm-random-number-generation/)

Happy coding in Elm 🎉!