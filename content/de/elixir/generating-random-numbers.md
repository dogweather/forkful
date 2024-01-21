---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:43.056843-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen zu erzeugen bedeutet, nicht vorhersagbare Nummern in deinem Code zu generieren. Programmierer nutzen das für Spiele, Simulationen, Tests oder um die Sicherheit (wie in Verschlüsselungen) zu erhöhen.

## So geht's:
```elixir
# Elixir nutzt das :rand Modul
:rand.seed(:exsplus) # Seed setzen – nur einmal nötig
zufallszahl = :rand.uniform() # Eine Zufallszahl zwischen 0 und 1
IO.puts(zufallszahl)

# Für einen Bereich – hier: 1 bis 10
zehn_zufallszahlen = Enum.map(1..10, fn _ -> :rand.uniform(10) end)
IO.inspect(zehn_zufallszahlen)
```
Beispiel Ausgabe:
```
0.4435837278580607
[6, 2, 10, 4, 3, 7, 5, 8, 9, 1]
```

## Tiefgang:
Zufallszahlen in Elixir sind nicht wirklich "zufällig", sondern "Pseudozufallszahlen" – vom Computer generiert und daher vorhersagbar, wenn du den Seed kennst. Historisch gab’s viele Algorithmen dafür, :exsplus (Exponential Squared) ist einer der neueren und liefert eine gute Balance zwischen Geschwindigkeit und Zufälligkeit. Alternativen wie :exrop und :exsss bieten unterschiedliche Charakteristika und Distributionen. Für kryptografische Sicherheit sollte aber kein Pseudozufallsgenerator, sondern ein kryptografisch sicherer Algorithmus genutzt werden.

## Siehe auch:
- Elixir-Dokumentation zum :rand Modul: https://hexdocs.pm/elixir/1.12/Random.html
- Erlang-Dokumentation von :rand: http://erlang.org/doc/man/rand.html
- Diskussion über Pseudozufallszahlen: https://stackoverflow.com/questions/27643616/elixir-random-number-generation