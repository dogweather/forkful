---
aliases:
- /pl/elixir/working-with-json/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.876975-07:00
description: "Praca z JSON polega na parsowaniu ci\u0105g\xF3w znak\xF3w w formacie\
  \ JSON na struktury danych, kt\xF3re Elixir mo\u017Ce manipulowa\u0107, oraz na\
  \ serializacji struktur danych\u2026"
lastmod: 2024-02-18 23:08:49.327641
model: gpt-4-0125-preview
summary: "Praca z JSON polega na parsowaniu ci\u0105g\xF3w znak\xF3w w formacie JSON\
  \ na struktury danych, kt\xF3re Elixir mo\u017Ce manipulowa\u0107, oraz na serializacji\
  \ struktur danych\u2026"
title: Praca z JSON
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z JSON polega na parsowaniu ciągów znaków w formacie JSON na struktury danych, które Elixir może manipulować, oraz na serializacji struktur danych Elixir z powrotem na ciągi znaków JSON. Jest to niezbędne do rozwoju stron internetowych, API i plików konfiguracyjnych, ponieważ JSON jest lekkim, tekstowym, niezależnym od języka formatem wymiany danych, szeroko stosowanym ze względu na swoją prostotę i czytelność dla człowieka.

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
