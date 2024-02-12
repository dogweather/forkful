---
title:                "Gebruik van associatieve arrays"
aliases:
- /nl/elixir/using-associative-arrays/
date:                  2024-01-30T19:10:50.895247-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van associatieve arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

In Elixir worden associatieve arrays, genaamd Maps, gebruikt als collecties van sleutel-waardeparen waar een unieke sleutel naar een waarde wijst. Ze zijn enorm handig voor het opslaan en ophalen van gegevens onderweg, waardoor je code schoner wordt en je leven makkelijker.

## Hoe te:

Een Map maken is eenvoudig. Je gebruikt de `%{}` syntaxis, zo:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Waarden benaderen doe je door de sleutels te gebruiken:

```elixir
IO.puts my_map["name"]
```
Output: `Alex`

Om waarden toe te voegen of bij te werken, kun je de `Map.put/3` functie gebruiken:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Output: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Sleutels verwijderen is net zo eenvoudig met `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Output: `%{"location" => "NY", "name" => "Alex"}`

## Diepere Duik

Maps in Elixir zijn een evolutie van de oudere sleutel-waardeopslagtypes, zoals Hashes in Ruby of Dictionaries in Python. Ze zorgen voor efficiëntere zoekopdrachten en invoegingen, waardoor ze de voorkeur krijgen voor modern Elixir-programmeren. Het is opmerkelijk dat voor de Maps, Elixir gebruikmaakte van de HashDict en Dict modules, die nu verouderd zijn.

Echter, voor scenario's waar geordende gegevens nodig zijn, kunt u kijken naar trefwoordlijsten in Elixir. Dit zijn lijsten van tuples, efficiënt voor kleinere verzamelingen maar niet zo prestatievriendelijk voor grote datasets als Maps.

Houd er rekening mee dat Maps hun sleutels in een "platte" structuur opslaan, waardoor directe toegang tot geneste waarden een beetje lastig kan zijn. Voor diepe nesteling kun je overwegen om gestructureerde toegang te gebruiken via de `get_in`, `put_in`, `update_in` en `get_and_update_in` functies, die een dynamischere benadering van geneste gegevensmanipulatie mogelijk maken.

Samengevat, terwijl Maps je go-to zijn voor behoeften aan associatieve arrays in Elixir, biedt de taal een rijke verscheidenheid aan gegevensstructuren voor elk scenario, waarbij je wordt aangemoedigd om het juiste gereedschap voor de klus te kiezen.
