---
title:                "Praca z plikami CSV"
aliases:
- pl/elixir/working-with-csv.md
date:                  2024-02-03T19:19:47.684348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z plikami CSV (Comma-Separated Values - wartości oddzielone przecinkami) polega na odczycie z tych plików i zapisywaniu danych do nich, co jest często wymagane przy zadaniach związanych z importem/eksportem danych lub prostymi rozwiązaniami przechowywania. Programiści wykorzystują tę funkcjonalność do wymiany danych między systemami, szybkiej edycji danych lub w sytuacjach, gdy lekki i łatwy do manipulacji format danych jest zaletą.

## Jak to zrobić:

Elixir, z jego potężnym dopasowywaniem wzorców i wsparciem dla łączenia potokowego (pipelining), może skutecznie obsłużyć pliki CSV, nawet bez zewnętrznych bibliotek. Jednakże, dla bardziej zaawansowanych potrzeb, biblioteka `nimble_csv` jest szybkim i prostym wyborem.

### Odczyt pliku CSV bez zewnętrznych bibliotek

Możesz odczytać i zanalizować plik CSV za pomocą wbudowanych funkcji Elixira:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Przykładowe użycie
CSVReader.read_file("data.csv")
# Wyjście: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### Zapis do pliku CSV

Podobnie, aby zapisać dane do pliku CSV:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# Przykładowe użycie
dane = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", dane)
# Tworzy plik output.csv z danymi sformatowanymi jako CSV
```

### Używanie `nimble_csv`

Dla bardziej skomplikowanego obsługiwania plików CSV, `nimble_csv` oferuje potężny i elastyczny sposób pracy z danymi CSV. Najpierw dodaj `nimble_csv` do twoich zależności w `mix.exs` i uruchom `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Analiza danych CSV z `nimble_csv`:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Przykładowe użycie
MyCSVParser.parse("data.csv")
# Wyjście z nimble_csv można dostosować w zależności od definicji, ale ogólnie wygląda to jak lista list lub krotek, w zależności od tego, jak skonfigurowałeś parser.
```

Zapis danych CSV za pomocą `nimble_csv` wymaga ręcznej transformacji danych na odpowiedni format, a następnie zapisania ich do pliku, podobnie jak w prostym przykładzie Elixira, ale wykorzystując `nimble_csv` do generowania poprawnie sformatowanych wierszy CSV.

Wybierając odpowiednie podejście do złożoności twojego zadania, możesz obsługiwać pliki CSV w Elixirze z dużą elastycznością i mocą.
