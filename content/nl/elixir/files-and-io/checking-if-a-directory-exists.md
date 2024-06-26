---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:57.907089-07:00
description: 'Hoe te: Elixir''s `File` module is je beste vriend voor mapcontroles.
  Gebruik `File.dir?/1` om een boolean terug te geven die aangeeft of de map bestaat.'
lastmod: '2024-03-13T22:44:50.476555-06:00'
model: gpt-4-0125-preview
summary: Elixir's `File` module is je beste vriend voor mapcontroles.
title: Controleren of een directory bestaat
weight: 20
---

## Hoe te:
Elixir's `File` module is je beste vriend voor mapcontroles. Gebruik `File.dir?/1` om een boolean terug te geven die aangeeft of de map bestaat.

```elixir
# Controleer of map bestaat
if File.dir?("/pad/naar/map") do
  IO.puts("Map bestaat!")
else
  IO.puts("Zulke map bestaat niet.")
end
```

Voorbeelduitvoer voor een bestaande map:
```elixir
Map bestaat!
```

Voorbeelduitvoer voor een niet-bestaande map:
```elixir
Zulke map bestaat niet.
```

## Diepere Duik
Historisch gezien hebben bestandssysteemoperaties in programmering een belang gedragen vanwege de behoefte aan het lezen/schrijven van gegevens. In Elixir heeft de `File` module deze operaties netjes geabstraheerd. Het draait allemaal om betrouwbaarheid met deze controles; daardoor is `File.dir?/1` een basis voor het verifiëren van paden.

Alternatieven voor `File.dir?/1` kunnen zijn het gebruiken van `File.stat/2` en controleren of het resultaat `:ok` is, wat aangeeft dat de map bestaat. Een andere benadering kan zijn het gebruik van `:filelib.is_dir/1` uit de standaardbibliotheek van Erlang, die Elixir kan benutten vanwege zijn interoperabiliteit met Erlang.

Elixir's implementatie van het controleren of een map bestaat, bouwt voort op Erlang's robuuste bestandshandeling. Dit ontwerp maakt gebruik van het BEAM's vermogen voor fouttolerante systemen, waarin Elixir-toepassingen doorgaans draaien.

## Zie Ook
- Documentatie van Elixir's `File` module: [https://hexdocs.pm/elixir/File.html](https://hexdocs.pm/elixir/File.html)
- Documentatie van Erlang's `filelib` module voor meer bestandssysteemfuncties: [http://erlang.org/doc/man/filelib.html](http://erlang.org/doc/man/filelib.html)
- Robuuste bestandshandeling in Elixir: [https://elixir-lang.org/getting-started/io-and-the-file-system.html](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
