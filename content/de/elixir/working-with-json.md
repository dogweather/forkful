---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für JavaScript Object Notation. Entwickler nutzen es, weil es ein leichtgewichtiges Format für den Austausch von Daten zwischen Servern und Web-Apps ist. Es ist einfach zu lesen und zu schreiben.

## How to:
Arbeiten mit JSON in Elixir erfordert das `Jason` Paket. Füge `{:jason, "~> 1.2"}` zu deiner `mix.exs` hinzu und führe `mix deps.get` aus. Hier sind ein paar Beispiele:

```elixir
# Dezimal nach JSON konvertieren
iex> map = %{name: "Elixir", awesome: true}
iex> Jason.encode!(map)
"{\"name\":\"Elixir\",\"awesome\":true}"

# JSON parsen
iex> json = "{\"language\":\"Elixir\",\"rating\":5}"
iex> Jason.decode!(json)
%{"language" => "Elixir", "rating" => 5}
```

## Deep Dive
JSON wurde Anfang der 2000er als einfachere Alternative zu XML entworfen. Neben `Jason` könntest du auch `Poison` als Elixir-Paket nutzen. Elixir nutzt binäre Patter-Matching, was JSON-Parsing besonders effizient macht.

## See Also
- [Jason GitHub repository](https://github.com/michalmuskala/jason)
- [Poison - Ein weiteres JSON-Paket für Elixir](https://github.com/devinus/poison)
