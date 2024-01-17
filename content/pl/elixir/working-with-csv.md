---
title:                "Praca z plikiem csv"
html_title:           "Elixir: Praca z plikiem csv"
simple_title:         "Praca z plikiem csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

##Co i Dlaczego?
Praca z CSV to nic innego jak manipulowanie danymi w formacie tekstowym, gdzie wartości są oddzielone przecinkami. Programiści często używają tego formatu do importowania i eksportowania danych w aplikacjach.

##Jak to zrobić:
Aby pracować z CSV w Elixirze, możemy użyć biblioteki CSV. Pierwszym krokiem jest jej zainstalowanie za pomocą mix:
```Elixir
mix deps.get csv
```
Następnie, możemy zaimportować bibliotekę przy użyciu:
```Elixir
iex> {:ok, csv} = File.read("data.csv") |> CSV.decode(delimiter: "\t")
```
Teraz możemy przetwarzać nasze dane tak jak chcemy, na przykład:
```Elixir
iex> csv |> CSV.decode |> Enum.take(5)
```
Wyjściem będzie tablica z pierwszymi pięcioma wierszami naszego pliku CSV.

##Głęboka Fuzja:
Format CSV został stworzony w celu umożliwienia wymiany danych między różnymi systemami. Jest on powszechnie używany w arkuszach kalkulacyjnych, bazach danych czy programach analitycznych. Alternatywnymi formatami są między innymi JSON i XML.

Biblioteka CSV w Elixirze jest bardzo wydajna i obsługuje różne opcje jak na przykład zmiana separacji, nagłówka czy obsługa baz danych. Można jej używać zarówno w prostych skryptach, jak i w dużych aplikacjach.

##Zobacz także:
- Dokumentacja biblioteki CSV w Elixirze: https://hexdocs.pm/csv/CSV.html
- Wprowadzenie do formatu CSV: https://www.ionos.com/digitalguide/websites/web-development/csv-file/
- Porównanie różnych formatów danych: https://www.programming-idiots.com/choose-right-data-format/