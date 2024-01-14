---
title:                "Elixir: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV jest nieodłączną częścią wielu projektów programistycznych. Elixir oferuje wiele narzędzi i funkcji, które ułatwiają pracę z tym formatem danych. W tym artykule dowiecie się, dlaczego jest to wartościowa umiejętność dla każdego programisty.

## Jak to zrobić

Przed rozpoczęciem pracy z plikami CSV w Elixirze, musimy zainstalować bibliotekę [CSV](https://hex.pm/packages/csv). Możemy to zrobić za pomocą menadżera pakietów Hex korzystając z poniższej komendy w terminalu:

```
mix deps.get
```

Po zainstalowaniu biblioteki, możemy łatwo wczytywać i zapisywać dane w formacie CSV za pomocą jej funkcji. Przykładowy kod wyglądałby następująco:

```elixir
require CSV
data = ["Jan", "Kowalski", "30"]
CSV.encode(data, headers: false)
```

Powinno to zwrócić następujący wynik:

```
"Jan,Kowalski,30"
```

## Głębsze zagadnienia

Oprócz podstawowych funkcji do zapisu i odczytu danych, biblioteka CSV oferuje również możliwość pracy z plikami CSV o różnych separatorach, kodowaniach, a także obsługiwać pliki z nagłówkami. Dodatkowo, istnieje również możliwość manipulacji i przetwarzania danych w formacie CSV za pomocą funkcji takich jak `CSV.parse/2` czy `CSV.map_rows/2`. Szczegółowa dokumentacja biblioteki dostępna jest na [oficjalnej stronie](https://hexdocs.pm/csv/api-reference.html).

## Zobacz również

- [Dokumentacja biblioteki CSV](https://hexdocs.pm/csv/api-reference.html)
- [Wstęp do programowania w Elixirze](https://medium.com/@Dragonza/introduction-to-elixir-programming-language-2bae3dc9e699)
- [Oficjalna strona Elixir](https://elixir-lang.org/)