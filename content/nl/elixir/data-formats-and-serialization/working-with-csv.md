---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:54.982802-07:00
description: "Hoe: Elixir bevat geen CSV-parsering in zijn standaardbibliotheek, maar\
  \ je kunt het `CSV` hex-pakket gebruiken. Hier is een snel voorbeeld om je op weg\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.484215-06:00'
model: gpt-4-0125-preview
summary: Elixir bevat geen CSV-parsering in zijn standaardbibliotheek, maar je kunt
  het `CSV` hex-pakket gebruiken.
title: Werken met CSV
weight: 37
---

## Hoe:
Elixir bevat geen CSV-parsering in zijn standaardbibliotheek, maar je kunt het `CSV` hex-pakket gebruiken. Hier is een snel voorbeeld om je op weg te helpen:

```elixir
# Voeg eerst `{:csv, "~> 2.4"}` toe aan je mix.exs en voer `mix deps.get` uit
# Gebruik dan het CSV-module als volgt:

CSV.decode("naam,leeftijd\nJohn Doe,27\nJane Smith,32", headers: true)
|> Enum.map(fn(rij) -> 
  "Hallo, #{rij["naam"]} die #{rij["leeftijd"]} jaar oud is!"
end)
```

Voorbeelduitvoer:

```
["Hallo, John Doe die 27 jaar oud is!", "Hallo, Jane Smith die 32 jaar oud is!"]
```

## Diepere Duik
CSV is niet nieuw; het bestaat al sinds de vroege jaren 1970, waardoor het een van de meest blijvende bestandsformaten is. De eenvoud is zowel zijn grootste kracht als zijn grootste zwakte. Alternatieven zijn JSON, XML of binaire formaten zoals Protocol Buffers, elk met hun afwegingen in complexiteit, grootte en leesbaarheid. Wat betreft Elixir, wanneer je CSV-gegevens decodeert met het `CSV`-pakket, worden onder de motorkap veelvoorkomende problemen zoals gegevenstypeconversie, escapes en tekenencoding naadloos afgehandeld.

## Zie Ook
- De documentatie van het `CSV` hex-pakket: <https://hexdocs.pm/csv>
- Een inleiding tot Elixir's Stream-module voor grote CSV's: <https://elixir-lang.org/getting-started/enumerables-and-streams.html>
- Een vergelijking van bestandsformaten (CSV, JSON, XML, enz.): <https://www.ibm.com/docs/en/iis/11.5?topic=formats-comparing-file-reactivity>
