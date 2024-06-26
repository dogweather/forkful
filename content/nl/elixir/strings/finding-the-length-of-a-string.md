---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:55.067717-07:00
description: 'Hoe: In Elixir krijg je de lengte van een string met de `String.length/1`
  functie. Zo werkt het.'
lastmod: '2024-03-13T22:44:50.452916-06:00'
model: gpt-4-0125-preview
summary: In Elixir krijg je de lengte van een string met de `String.length/1` functie.
title: De lengte van een string vinden
weight: 7
---

## Hoe:
In Elixir krijg je de lengte van een string met de `String.length/1` functie. Zo werkt het:

```elixir
my_string = "Hallo, Wereld!"
length = String.length(my_string)
IO.puts(length)
```

Voorbeelduitvoer:

```
13
```

## Diepgaand
Intern zijn Elixir strings UTF-8 gecodeerde binaries. Elk karakter kan een tot vier bytes zijn. Dus, wanneer we `String.length/1` aanroepen, tellen we niet zomaar bytes; we tellen Unicode grafemen, wat we waarnemen als karakters.

Historisch gezien waren stringlengtebewerkingen in veel talen byte-gecentreerd en hielden ze niet goed rekening met multi-byte karakters. Elixir's benadering is modern en vanaf het begin Unicode-vriendelijk.

Wat betreft alternatieven, je zou handmatig grafemen kunnen tellen met behulp van recursie of met een lus, maar dat is onnodig en inefficiënt. `String.length/1` is geoptimaliseerd en idiomatisch.

Elixir's implementatie gebruikt een Erlang NIF (Native Implemented Function) voor `String.length/1`, waardoor het bliksemsnel is. Bytes tellen in plaats van grafemen wordt gedaan met `byte_size/1`, wat de ruwe bytes van de binaire representatie van een string telt - nuttig in low-level operaties waar codering niet uitmaakt.

## Zie Ook
- [Documentatie van Elixir's String-module](https://hexdocs.pm/elixir/String.html)
- [Unicode Standaard](http://www.unicode.org/standard/standard.html)
