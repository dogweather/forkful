---
title:                "Generierung von Zufallszahlen"
aliases: - /de/elixir/generating-random-numbers.md
date:                  2024-01-27T20:32:58.266653-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen in Elixir zu erzeugen, ist eine grundlegende Programmieraufgabe, die für Anwendungen unerlässlich ist, die unvorhersehbare Ergebnisse benötigen, wie beispielsweise die Erstellung sicherer Tokens, das Stichprobenziehen von Daten oder in Spielalgorithmen. Programmierer nutzen dies, um ein Maß an Zufälligkeit und Variabilität in ihren Anwendungen einzuführen, was sie dynamischer und weniger deterministisch macht.

## Wie:

Um Zufallszahlen in Elixir zu generieren, verwenden Sie in erster Linie das `:rand` Modul, das mehrere Funktionen für diesen Zweck bereitstellt. Hier ist eine schnelle Anleitung, um zu beginnen:

Zuerst stellen Sie sicher, dass Sie den Zufallszahlengenerator initialisieren, indem Sie ihn mit einem einzigartigen Startpunkt säen:

```elixir
:rand.seed(:exsplus)
```

Um eine zufällige ganze Zahl innerhalb eines Bereichs zu generieren, verwenden Sie:

```elixir
random_integer = :rand.uniform(10) # Generiert eine Zahl zwischen 1 und 10
IO.puts(random_integer)
```

Für eine zufällige Float-Zahl zwischen 0 und 1.0:

```elixir
random_float = :rand.uniform()
IO.puts(random_float)
```

Für einen spezifischeren Bereich von Floats benötigen Sie etwas mehr Berechnung:

```elixir
min = 1.5
max = 5.5
random_float_range = min + (:rand.uniform() * (max - min))
IO.puts(random_float_range)
```

Denken Sie daran, dass diese Zahlen pseudozufällig sind; sie werden durch den Seed und den Algorithmus bestimmt, reichen aber für die meisten Anwendungen aus.

## Tiefergehend

Die Fähigkeit von Elixir, Zufallszahlen zu generieren, stützt sich auf das `:rand` Modul von Erlang, was sein Erbe und die enge Beziehung zu Erlang widerspiegelt. Das `:rand` Modul ersetzte das ältere `:random` Modul, und bietet verbesserte Algorithmen für die Generierung von Zufallszahlen. Es liefert eine Vielfalt von Algorithmen; der Standard ist `exsplus`, unterstützt aber auch andere wie `exs64`, `exsl` und mehr, wobei jeder seine Kompromisse hinsichtlich Geschwindigkeit und Qualität der Zufälligkeit hat.

Ein interessanter Aspekt der Zufallszahlengenerierung in Elixir (und somit auch in Erlang) ist sein Umgang mit Seeds. Das System unterhält separate Seed-Zustände für jeden Prozess, was sicherstellt, dass gleichzeitige Prozesse nicht mit den Zufallszahlenfolgen der anderen interferieren. Dies ist besonders nützlich in nebenläufigen Anwendungen, um Vorhersagbarkeit und Zuverlässigkeit in verteilten Systemen zu gewährleisten.

Während das `:rand` Modul für die meisten Anwendungsfälle ausreicht, sollten Anwendungen, die kryptographisch sichere Zufallszahlen benötigen, andere Optionen in Betracht ziehen. Das `crypto` Modul bietet Funktionen wie `crypto:strong_rand_bytes/1`, die darauf ausgelegt sind, sichere zufällige Daten für kryptographische Zwecke zu generieren. Diese Alternativen sind essentiell für sicherheitskritische Anwendungen wie Token-Generierung, Verschlüsselung und bestimmte Arten von Authentifizierungsmechanismen.
