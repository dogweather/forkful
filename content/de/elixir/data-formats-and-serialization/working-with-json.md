---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.203794-07:00
description: "Die Arbeit mit JSON umfasst das Parsen von JSON-formatierten Strings\
  \ in Datenstrukturen, die Elixir manipulieren kann, und das Serialisieren von Elixir-\u2026"
lastmod: '2024-03-11T00:14:27.450138-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit JSON umfasst das Parsen von JSON-formatierten Strings in\
  \ Datenstrukturen, die Elixir manipulieren kann, und das Serialisieren von Elixir-\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was und Warum?

Die Arbeit mit JSON umfasst das Parsen von JSON-formatierten Strings in Datenstrukturen, die Elixir manipulieren kann, und das Serialisieren von Elixir-Datenstrukturen zurück in JSON-Strings. Dies ist wesentlich für die Webentwicklung, APIs und Konfigurationsdateien, da JSON ein leichtgewichtiges, textbasiertes, sprachunabhängiges Datenaustauschformat ist, das aufgrund seiner Einfachheit und Menschenlesbarkeit weit verbreitet verwendet wird.

## Wie:

In Elixir kann man die `Jason` Bibliothek verwenden, eine beliebte Wahl für JSON-Parsing und -Generierung. Fügen Sie zuerst `Jason` zu den Abhängigkeiten Ihres Projekts in `mix.exs` hinzu:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Führen Sie dann `mix deps.get` aus, um die Abhängigkeit zu holen.

### JSON parsen:
Um einen JSON-String in Elixir-Datenstrukturen umzuwandeln:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Ausgabe: %{"name" => "John", "age" => 30}
```

### JSON erzeugen:
Um eine Elixir-Map in einen JSON-String zu konvertieren:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Ausgabe: {"age":25,"name":"Jane"}
```

### Mit Strukturen arbeiten:
Um eine Elixir-Struktur zu kodieren, müssen Sie das `Jason.Encoder` Protokoll für Ihre Struktur implementieren. Hier ein Beispiel:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Ausgabe: {"age":28,"name":"Mike"}
```

Dieser einfache Ansatz wird Ihnen den Einstieg in die Integration der JSON-Verarbeitung in Ihre Elixir-Anwendungen erleichtern und den Datenaustausch in verschiedenen Programmierumgebungen ermöglichen.
