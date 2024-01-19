---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen generieren beschäftigt sich mit der Erstellung von Zahlen, die keinen Zusammenhang haben, also zufällig sind. Programmierer tun dies, um Datensätze zu simulieren, Spiele zu erzeugen oder ungeordnete Elemente anzuzeigen. 

## So geht's:

Erzeugen Sie eine einfache Zufallszahl mit der `random`-Funktion des Erlang-Moduls wie folgt:
```elixir
:rand.uniform(10) # Gibt eine Zufallszahl zwischen 1 und 10 zurück
```

Schauen wir uns einen Beispielausdruck an:
```elixir
IO.puts(:rand.uniform(10)) # Kann irgendeine Zahl zwischen 1 und 10 ausgeben, z.B. 3
```

## Tiefer tauchen:

Historisch gesehen, nutzte Erlang (und somit Elixir) das :random-Modul zur Erzeugung von Zufallszahlen. Es wurde jedoch durch das :rand-Modul ersetzt, das bessere zufällige Verteilungen ermöglicht.

Es gibt auch eine Option, den Zufallszahlengenerator zu erzeugen und zu verwalten. Zum Beispiel:
```elixir
{:ok, s} = :rand.make_seed(:exrop)
```
`make_seed/1` erzeugt einen Anfangszustand für den Zufallszahlengenerator, wobei `:exrop` das Exponential Roulette Wheel Parent Atom ist. Der Generator kann dann mit `uniform/1` verwendet werden, z.B. `:rand.uniform(100, s)`.

## Siehe auch:

Weitere Informationen finden Sie unter den folgenden Links:
Erlang :rand-Modul Dokumentation: (http://erlang.org/doc/man/rand.html)
Elixir-Schulungszentrum: (https://elixir-lang.org/getting-started/introduction.html)
Offizielles Forum für Elixir-Entwicklung und -Diskussionen: (https://elixirforum.com/)