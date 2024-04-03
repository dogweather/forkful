---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.876975-07:00
description: "Jak to zrobi\u0107: W Elixirze mo\u017Cesz u\u017Cy\u0107 biblioteki\
  \ `Jason`, popularnego wyboru do parsowania i generowania JSON. Najpierw dodaj `Jason`\
  \ do zale\u017Cno\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.066256-06:00'
model: gpt-4-0125-preview
summary: "W Elixirze mo\u017Cesz u\u017Cy\u0107 biblioteki `Jason`, popularnego wyboru\
  \ do parsowania i generowania JSON."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
W Elixirze możesz użyć biblioteki `Jason`, popularnego wyboru do parsowania i generowania JSON. Najpierw dodaj `Jason` do zależności Twojego projektu w `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Następnie uruchom `mix deps.get`, aby pobrać zależność.

### Parsowanie JSON:
Aby przekonwertować ciąg znaków JSON na struktury danych Elixir:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Wynik: %{"name" => "John", "age" => 30}
```

### Generowanie JSON:
Aby przekonwertować mapę Elixir na ciąg znaków JSON:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Wynik: {"age":25,"name":"Jane"}
```

### Praca ze strukturami:
Aby zakodować strukturę Elixir, musisz zaimplementować protokół `Jason.Encoder` dla swojej struktury. Oto przykład:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Wynik: {"age":28,"name":"Mike"}
```

To proste podejście pozwoli Ci zacząć integrować przetwarzanie JSON do Twoich aplikacji Elixir, ułatwiając wymianę danych w różnych środowiskach programistycznych.
