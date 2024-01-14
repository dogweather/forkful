---
title:                "Elixir: Erzeugung von Zufallszahlen"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

#Warum

Warum sollte man sich überhaupt mit der Generierung von Zufallszahlen beschäftigen? Nun, Zufallszahlen werden oft in der Programmierung verwendet, um verschiedene Ergebnisse und Verhaltensweisen in Programmen zu erzeugen. Sie können zum Beispiel zur Simulation von Spielen, für kryptografische Zwecke oder zur Erzeugung von eindeutigen IDs verwendet werden.

##Wie funktioniert es?

In Elixir gibt es verschiedene Möglichkeiten, um Zufallszahlen zu generieren. Eine Möglichkeit ist die Verwendung der `:random` Modul, der verschiedene Funktionen bietet, um Zufallszahlen zu erzeugen. Hier ist ein Beispielcode für die Erzeugung einer Zufallszahl zwischen 1 und 10:

```Elixir
:random.uniform(1, 10)
```

Die Ausgabe könnte zum Beispiel `7` sein.

Der `:random` Modul bietet auch Funktionen für die Erzeugung von Zufallszahlen mit bestimmten Verteilungen, wie zum Beispiel der Gaußschen Verteilung oder der gleichmäßigen Verteilung.

##Tiefergehende Infos

Die Random Number Generation (RNG) ist ein komplexes Thema in der Informatik und es gibt viele verschiedene Algorithmen und Techniken, um Zufallszahlen zu erzeugen. Elixir verwendet den Mersenne-Twister-Algorithmus, um Zufallszahlen zu generieren. Dieser Algorithmus ist sehr effizient und erzeugt hochwertige Zufallszahlen.

Es ist auch wichtig zu wissen, dass Zufallszahlen, die von einem Computer erzeugt werden, nicht wirklich "zufällig" sind, da sie von Programmen erstellt werden. Sie sind jedoch stark genug für die meisten Anwendungen, bei denen eine gewisse Zufälligkeit erforderlich ist.

#Siehe auch

- [Elixir's official documentation on random](https://hexdocs.pm/elixir/Random.html)
- [A blog post on RNG algorithms in computer science](https://www.tutorialspoint.com/Random-Number-Algorithms-in-Computer-Science)
- [A tutorial on generating random numbers in Elixir](https://elixirstatus.com/p/CkCy-how-to-generate-random-numbers-in-elixir)