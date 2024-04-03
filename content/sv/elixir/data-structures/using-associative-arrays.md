---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:51.326344-07:00
description: "Hur man g\xF6r: Att skapa en Map \xE4r enkelt. Du anv\xE4nder `%{}`-syntaxen,\
  \ s\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:37.559959-06:00'
model: gpt-4-0125-preview
summary: "Att skapa en Map \xE4r enkelt."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
Att skapa en Map är enkelt. Du använder `%{}`-syntaxen, så här:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Att komma åt värden görs genom att använda nycklarna:

```elixir
IO.puts my_map["name"]
```
Utskrift: `Alex`

För att lägga till eller uppdatera värden kan du använda funktionen `Map.put/3`:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Utskrift: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Att ta bort nycklar är lika enkelt med `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Utskrift: `%{"location" => "NY", "name" => "Alex"}`

## Fördjupning
Maps i Elixir är en evolution av de äldre nyckel-värde-lagrings typerna, som Hashes i Ruby eller Dictionaries i Python. De möjliggör effektivare sökningar och insättningar, vilket gör dem till ett självklart val för modern Elixir-programmering. Det är värt att notera att före Maps använde Elixir HashDict och Dict-moduler, vilka är föråldrade nu.

Dock, för scenarier som kräver ordnade data, kan du titta på nyckelordlistor i Elixir. Dessa är listor av tupler, effektiva för mindre samlingar men inte lika prestandavänliga för stora datamängder som Maps.

Ha i åtanke att Maps lagrar sina nycklar i en "platt" struktur, vilket gör direkt tillgång till nästlade värden lite knepigt. För djup nästling kan du överväga strukturerad åtkomst via funktionerna `get_in`, `put_in`, `update_in`, och `get_and_update_in`, vilka tillåter en mer dynamisk ansats till manipulation av nästlade data.

Sammanfattningsvis, medan Maps är ditt självklara val för behov av associativa arrayer i Elixir, erbjuder språket en rik variation av datastrukturer för varje scenario, vilket uppmuntrar dig att välja det rätta verktyget för jobbet.
