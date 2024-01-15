---
title:                "Praca z plikami csv"
html_title:           "Elixir: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub programistką, prawdopodobnie spotkałeś się z formatem pliku CSV (Comma-Separated Values) przy pracy z danymi. Może wydawać się on nieco mniej popularny niż inne formaty, ale wciąż jest bardzo użyteczny do przechowywania i przetwarzania informacji w prosty i czytelny sposób. W tym artykule dowiesz się, jak używać języka Elixir do obsługi plików CSV.

## Jak to zrobić

Elixir ma wbudowaną bibliotekę CSV, która pomaga w łatwy sposób zarządzać plikami CSV. Aby rozpocząć pracę, musisz najpierw zainstalować pakiet w swoim projekcie Elixir. Aby to zrobić, odpal następujące komendy:

```Elixir 
mix deps.get
mix deps.clean csv
mix deps.get json_struct
```

Pakiet json_struct jest opcjonalny, ale jest przydatny do formatowania wyjścia w terminalu.

Następnie, wystarczy, że wczytasz plik CSV używając wbudowanej funkcji ```Elixir CSV.parse ``` i przekażesz jej ścieżkę do pliku. Na przykład:

```Elixir
parsed = CSV.parse("dane.csv")
```

Funkcja zwróci listę list, gdzie każda lista reprezentuje wiersz w pliku CSV. Możesz również przekazać opcje do funkcji, aby dostosować jej działanie. Na przykład, jeśli w twoim pliku CSV pierwszy wiersz zawiera nagłówki kolumn, możesz przekazać opcję ```Elixir [:headers] ``` aby przypisać nazwy kolumn do każdego wiersza. Przykład:

```Elixir
parsed = CSV.parse("dane.csv", [:headers])
```

Inne przydatne opcje do funkcji ```parse ``` to między innymi ```Elixir [:trim] ``` do usuwania białych znaków z danych i ```Elixir [:converters] ``` do konwertowania typów danych.

Jeśli chcesz zapisać dane z listy list do pliku CSV, możesz użyć funkcji ```Elixir CSV.encode ```. Przykład:

```Elixir
CSV.encode("nowe_dane.csv", parsed)
```

Funkcja ta przyjmie nazwę pliku jako pierwszy argument, a dane jako drugi.

## Głębszy zanurkować

W powyższych przykładach, omówiliśmy podstawową obsługę plików CSV za pomocą wbudowanej biblioteki CSV. Jednak, istnieje wiele innych bibliotek, które mogą ułatwić pracę z tym formatem. Na przykład, biblioteka esv jest często wybierana ze względu na szybkość przetwarzania danych.

Innym przydatnym narzędziem jest kompilator plików CSV, który przetwarza pliki CSV w kod Elixir. Dzięki temu, możesz używać funkcji i wzorców Elixir do manipulowania danymi i tworzenia dynamicznych raportów.

## Zobacz też

- [Biblioteka CSV dla Elixir](https://hexdocs.pm/csv/CSV.html)
- [Biblioteka esv](https://github.com/beatrichartz/csv)
- [Kompilator plików CSV dla Elixir](https://github.com/fazibear/csvex)