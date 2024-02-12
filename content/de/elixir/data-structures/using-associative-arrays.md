---
title:                "Verwendung von assoziativen Arrays"
aliases: - /de/elixir/using-associative-arrays.md
date:                  2024-01-30T19:10:32.042983-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verwendung von assoziativen Arrays"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

In Elixir sind assoziative Arrays, genannt Maps, Sammlungen von Schlüssel-Wert-Paaren, bei denen ein einzigartiger Schlüssel auf einen Wert zeigt. Sie sind super praktisch, um Daten unterwegs zu speichern und abzurufen, machen Ihren Code sauberer und Ihr Leben einfacher.

## Wie geht das:

Eine Map zu erstellen, ist unkompliziert. Sie verwenden die Syntax `%{}`, so wie hier:

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

Auf Werte zugreifen erfolgt durch Verwendung der Schlüssel:

```elixir
IO.puts my_map["name"]
```
Ausgabe: `Alex`

Um Werte hinzuzufügen oder zu aktualisieren, können Sie die Funktion `Map.put/3` verwenden:

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
Ausgabe: `%{"age" => 32, "location" => "NY", "name" => "Alex"}`

Schlüssel zu entfernen ist genauso einfach mit `Map.delete/2`:

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
Ausgabe: `%{"location" => "NY", "name" => "Alex"}`

## Tiefere Einblicke

Maps in Elixir sind eine Weiterentwicklung der älteren Schlüssel-Wert-Speichertypen, wie Hashes in Ruby oder Dictionaries in Python. Sie ermöglichen effizientere Suchvorgänge und Einfügungen und sind daher die erste Wahl für moderne Elixir-Programmierung. Es ist erwähnenswert, dass vor Maps, Elixir die Module HashDict und Dict verwendete, die nun veraltet sind.

Allerdings, für Szenarien, die geordnete Daten benötigen, könnten Sie sich Keyword-Listen in Elixir anschauen. Diese sind Listen von Tupeln, effizient für kleinere Sammlungen, aber nicht so leistungsfreundlich für große Datensätze wie Maps.

Beachten Sie, dass Maps ihre Schlüssel in einer "flachen" Struktur speichern, was den direkten Zugriff auf verschachtelte Werte etwas schwierig macht. Für tiefe Verschachtelungen könnten Sie strukturierten Zugriff über die Funktionen `get_in`, `put_in`, `update_in` und `get_and_update_in` in Betracht ziehen, welche einen dynamischeren Ansatz zur Manipulation verschachtelter Daten ermöglichen.

Zusammenfassend, während Maps Ihre erste Anlaufstelle für Bedürfnisse assoziativer Arrays in Elixir sind, bietet die Sprache eine reiche Vielfalt von Datenstrukturen für jedes Szenario und ermutigt Sie, das richtige Werkzeug für den Job auszuwählen.
