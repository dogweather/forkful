---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z JSON polega na przetwarzaniu struktur danych w formacie JSON, czyli lekkim formacie wymiany danych. Programiści robią to, żeby łatwiej komunikować się z API i wymieniać dane między różnymi systemami oraz językami programowania.

## Jak to zrobić:
Do obsługi JSON w Elixir używamy bibliotek, takich jak Jason. Poniżej przykład instalacji i użycia Jason:

```elixir
# Dodaj Jason do twojego mix.exs
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# Użycie Jason do kodowania i dekodowania JSON
defmodule JSONExample do
  def encode_to_json do
    data = %{name: "Alicja", age: 30}
    Jason.encode!(data)
  end

  def decode_from_json do
    json = ~S({"name": "Alicja", "age": 30})
    Jason.decode!(json)
  end
end

# Wypróbuj w iex:
iex> JSONExample.encode_to_json()
"{\"name\":\"Alicja\",\"age\":30}"
iex> JSONExample.decode_from_json()
%{"age" => 30, "name" => "Alicja"}
```

## Deep Dive:
Praca z JSON w Elixirze sięga czasów, gdy pierwsze wersje języka oferowały proste mechanizmy przetwarzania tekstów. Biblioteki takie jak Poison były powszechnie używane przed pojawieniem się Jason, który stał się standardem ze względu na swoją wydajność. Alternatywy dla Jason to m.in. Poison i JSX, ale Jason jest zalecany przez twórców Phoenix, popularnego frameworka Elixir. Implementacja Jason jest oparta na szybkim dekodowaniu i kodowaniu, minimalizując narzut pamięci.

## Zobacz również:
- Oficjalna dokumentacja Jason: https://hexdocs.pm/jason/readme.html
- Elixir School - Lekcje o JSON z Elixir: https://elixirschool.com/pl/lessons/serializations/json
- Poison - alternatywna biblioteka JSON dla Elixir: https://hexdocs.pm/poison/Poison.html
- Phoenix Framework - Użycie JSON w Phoenix: https://www.phoenixframework.org