---
title:    "Elixir: Zufallszahlen generieren"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von zufälligen Zahlen ist ein wichtiger Teil der Programmierung in Elixir. Mit zufälligen Zahlen können wir interessante und dynamische Features in unseren Anwendungen erstellen, wie z.B. zufällige Spielumgebungen, Kennwörter oder Farbkombinationen. In diesem Blog-Beitrag werden wir uns damit befassen, wie wir in Elixir zufällige Zahlen generieren können.

## Wie

Es gibt mehrere Möglichkeiten, um zufällige Zahlen in Elixir zu generieren.

#### Zufallszahlengenerator

Elixir verfügt über einen integrierten Zufallszahlengenerator, der auf dem Mersenne Twister Algorithmus basiert. Wir können diesen Generator verwenden, indem wir die `:random`-Bibliothek importieren und dann die Funktion `:random.uniform/1` aufrufen. Diese Funktion nimmt eine Zahl als Argument und gibt eine zufällige Zahl zwischen 0 und dieser Zahl zurück.

```Elixir
import :random

random_number = :random.uniform(10) #=> 3
```

#### Zufällige Zahlen mit Range

Eine weitere Möglichkeit, um zufällige Zahlen in Elixir zu generieren, ist die Verwendung von Ranges. Ranges sind eine aufzählbare Datentyp, der eine Sequenz von Werten darstellt. Wir können die Funktion `Enum.random/2` verwenden, um eine zufällige Zahl innerhalb eines Range zu erhalten.

```Elixir
random_number = Enum.random(1..10) #=> 8
```

#### Shuffle-Algorithmus

Mit dem `:random`-Modul können wir auch eine Liste von Elementen in zufälliger Reihenfolge sortieren. Dazu verwenden wir die Funktion `Enum.shuffle/1`.

```Elixir
list = [1, 2, 3, 4, 5]
shuffled_list = Enum.shuffle(list) #=> [3, 5, 1, 4, 2]
```

## Deep Dive

Das Generieren von zufälligen Zahlen mag auf den ersten Blick einfach klingen, aber es gibt verschiedene Faktoren, die bei der Wahl des richtigen Zufallszahlengenerators berücksichtigt werden müssen. So sollte z.B. der Algorithmus wirklich zufällige Werte generieren und keine vorhersehbaren Muster aufweisen. Auch die Effizienz und die Verteilung der Zufallszahlen müssen in Betracht gezogen werden. Ausführliche Informationen zu den verschiedenen Aspekten von Zufallszahlengeneratoren findet ihr in diesem [Artikel](https://erlangcentral.org/the-mersenne-twister-random-number-generator-in-elixir/).

## Siehe auch

- [Offizielle Elixir Dokumentation](https://elixir-lang.org/docs.html)
- [Elixir School](https://elixirschool.com/de/)
- [Elixir Forum](https://elixirforum.com/)