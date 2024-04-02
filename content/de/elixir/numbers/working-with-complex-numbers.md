---
date: 2024-01-26 04:38:48.117873-07:00
description: "Komplexe Zahlen bestehen aus einem Realteil und einem Imagin\xE4rteil\
  \ (wie `3 + 4i`). Sie werden in der Ingenieurwissenschaft, Physik und bei bestimmten\u2026"
lastmod: '2024-03-13T22:44:53.461662-06:00'
model: gpt-4-0125-preview
summary: "Komplexe Zahlen bestehen aus einem Realteil und einem Imagin\xE4rteil (wie\
  \ `3 + 4i`). Sie werden in der Ingenieurwissenschaft, Physik und bei bestimmten\u2026"
title: Umgang mit komplexen Zahlen
weight: 14
---

## Was & Warum?
Komplexe Zahlen bestehen aus einem Realteil und einem Imaginärteil (wie `3 + 4i`). Sie werden in der Ingenieurwissenschaft, Physik und bei bestimmten Computerproblemen verwendet. Programmierer arbeiten für Simulationen, Signalverarbeitung und zur effizienten Lösung bestimmter Mathematikprobleme mit ihnen.

## Wie:
Elixir bietet keine eingebauten komplexen Zahlen, daher müssen wir unsere eigenen erstellen oder eine Bibliothek wie `ComplexNum` verwenden. Hier ist ein schnelles Beispiel mit einer Bibliothek:

```elixir
# Angenommen, Sie haben ComplexNum installiert
defmodule ComplexMath do
  import ComplexNum

  def add(a, b) do
    ComplexNum.add(a, b)
  end
end

# Erstellen komplexer Zahlen und addieren dieser
c1 = {3, 4}   # steht für 3 + 4i
c2 = {2, -3}  # steht für 2 - 3i
ergebnis = ComplexMath.add(c1, c2)
IO.puts "Das Ergebnis ist: #{inspect(ergebnis)}"
```

Das würde ausgeben:
```
Das Ergebnis ist: {5, 1}
```

Das bedeutet, die Summe von `3 + 4i` und `2 - 3i` ist `5 + 1i`.

## Tiefer Eintauchen
Komplexe Zahlen tauchten in der Geschichte auf, weil normale alte Zahlen keine Quadratwurzeln aus negativen Zahlen ziehen konnten. Erst im 17. Jahrhundert wurden sie dank Mathematikern wie René Descartes und Gerolamo Cardano ernst genommen.

In Elixir verwendet man oft Tupel wie `{3, 4}` für komplexe Zahlen oder nutzt eine spezielle Bibliothek, um das Rad nicht neu erfinden zu müssen. Bibliotheken sind in der Regel besser - sie übernehmen die kniffligen Teile wie Multiplikation und Division, die aufgrund der imaginären Einheit 'i' (zur Info: `i` hoch 2 ergibt `-1`) heikel werden.

## Siehe auch
Schauen Sie sich diese Ressourcen an:
- [ComplexNum Library](https://hex.pm/packages/complex_num) für Elixirs Paketmanager, Hex.
- [Elixir School](https://elixirschool.com/en/), für fortgeschrittene Elixir-Themen und Übungen.
- [Erlang -- math Module](http://erlang.org/doc/man/math.html), das Elixir unter der Haube verwendet, für andere mathematische Bedürfnisse.
