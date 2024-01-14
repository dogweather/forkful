---
title:    "Elixir: Zufallszahlen generieren"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Warum

Wer kennt sie nicht, die Herausforderung, in der Programmierung eine Zufallszahl zu generieren? Sei es für ein Spiel, eine Simulation oder andere Anwendungen, das Erstellen von zufälligen Werten ist oft ein wichtiger Bestandteil des Codes. In diesem Blogbeitrag werden wir uns genauer ansehen, wie dies in Elixir funktioniert und warum es eine nützliche Technik für jede:n Programmierer:in ist.

# Wie es geht

Die Elixir-Standardbibliothek bietet die Funktion `:rand.uniform/1`, um eine zufällige Zahl zwischen 0 und 1 zu generieren. Dies kann durch Multiplikation mit dem gewünschten Bereich angepasst werden. Zum Beispiel kann `:rand.uniform/1` verwendet werden, um eine ganze Zufallszahl zwischen 0 und 10 zu generieren:

```elixir
:rand.uniform(10)
```

Elixir bietet auch die `:rand.seed/1` Funktion, um den sogenannten Seed-Wert für die Generation von Zufallszahlen festzulegen. Dies sorgt dafür, dass bei jedem Lauf des Programms die gleichen Zufallszahlen generiert werden. Zum Beispiel können wir den Seed-Wert auf 123 setzen und dann eine Liste von 10 zufälligen Zahlen zwischen 1 und 100 generieren:

```elixir
:rand.seed(123)
Enum.map(1..10, fn _ -> :rand.uniform(100) end)
```

Die Ausgabe könnte wie folgt aussehen:

```
[22, 78, 88, 57, 88, 1, 80, 12, 16, 29]
```

# Tieferer Einblick

Wenn wir uns genauer mit der `:rand.uniform/1` Funktion befassen, stellen wir fest, dass sie auf dem Mersenne-Twister-Algorithmus basiert, einem der am häufigsten verwendeten Algorithmen zur Generierung von Zufallszahlen. Dieser Algorithmus gewährleistet eine hohe Qualität der erzeugten Zufallszahlen.

Elixir bietet auch die `:rand.uniform/2` Funktion, die es ermöglicht, eine Liste von Zufallszahlen zu generieren. Zum Beispiel kann die folgende Codezeile verwendet werden, um 5 zufällige Gleitkommazahlen zwischen 0 und 1 zu generieren:

```elixir
:rand.uniform([0..1, 5])
```

Elixir bietet auch weitere Funktionen wie `:rand.normal/2` und `:rand.exponential/2`, um Zufallszahlen basierend auf verschiedenen Verteilungen zu generieren.

# Siehe auch

Weitere Informationen zur Verwendung von Zufallszahlen in Elixir finden Sie in der offiziellen Elixir-Dokumentation: 

- [Elixir-Dokumentation zu 'rand'](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#rand/0)
- [The Evolution of Randomness in Elixir - Code BEAM SF 2020 talk](https://www.youtube.com/watch?v=3grQWaZNMvg)
- [Elixir QuickTip: Randomness & Seed Values](https://elixir-lang.org/blog/2018/01/16/elixir-v1-6-0-released/#randomness_and_seed_values)
- [Elixir Randomness - Jose Valim blog](https://whitfin.io/elixir-randomness/)