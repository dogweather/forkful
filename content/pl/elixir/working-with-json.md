---
title:                "Elixir: Praca z formatem JSON"
simple_title:         "Praca z formatem JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z JSON w Elixirze pozwala na łatwą wymianę danych między różnymi aplikacjami i systemami. Jest to obecnie powszechny format danych, dlatego ważne jest, aby znać jego obsługę w tym języku programowania.

## Jak to zrobić

Kodowanie i dekodowanie danych JSON w Elixirze jest bardzo proste i wymaga użycia wbudowanych funkcji z biblioteki `Jason`. Oto przykładowy kod, który pokazuje, jak przetwarzać dane JSON:

```
stan = %{name: "John", age: 30, city: "Warsaw"}

# Kodowanie danych do formatu JSON
json_encoded = Jason.encode!(state)
IO.puts(json_encoded)
# Wynik: "{\"name\":\"John\",\"age\":30,\"city\":\"Warsaw\"}"

# Dekodowanie danych z formatu JSON
json_decoded = Jason.decode!(json_encoded)
IO.inspect(json_decoded)
# Wynik: %{name: "John", age: 30, city: "Warsaw"}
```

Jak widać powyżej, funkcje `encode/1` i `decode/1` są odpowiedzialne za kodowanie odpowiednio encodowanie i dekodowanie danych JSON. Ponadto, biblioteka `Jason` oferuje wiele innych funkcji, takich jak `decode!/2` czy `decode_with/2`, które pozwalają na bardziej zaawansowaną obsługę danych JSON.

## Głębsze zanurzanie

Elixir oferuje również bibliotekę `Poison`, która jest jednym z najbardziej popularnych sposobów na pracę z JSON. Często jest używana w celu obsługi błędów i nieprawidłowych danych wejściowych. Dodatkowo, w Elixirze istnieje również ważne pojęcie `protocoli`, które pozwalają na łatwe dodawanie własnej funkcjonalności do istniejących typów danych. Może to być bardzo użyteczne przy obsłudze formatu JSON w kodzie.

## Zobacz również

- [Dokumentacja Jason](https://hexdocs.pm/jason/readme.html)
- [Dokumentacja Poison](https://hexdocs.pm/poison/README.html)
- [Elixir Protocols](https://elixir-lang.org/getting-started/protocols.html)