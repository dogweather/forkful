---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:50.895247-07:00
description: 'Hoe te: Een Map maken is eenvoudig. Je gebruikt de `%{}` syntaxis, zo.'
lastmod: '2024-03-13T22:44:50.454974-06:00'
model: gpt-4-0125-preview
summary: Een Map maken is eenvoudig.
title: Gebruik van associatieve arrays
weight: 15
---

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
