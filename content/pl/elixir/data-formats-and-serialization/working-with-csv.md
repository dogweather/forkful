---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:47.684348-07:00
description: "Jak to zrobi\u0107: Elixir, z jego pot\u0119\u017Cnym dopasowywaniem\
  \ wzorc\xF3w i wsparciem dla \u0142\u0105czenia potokowego (pipelining), mo\u017C\
  e skutecznie obs\u0142u\u017Cy\u0107 pliki CSV, nawet\u2026"
lastmod: '2024-03-13T22:44:35.067342-06:00'
model: gpt-4-0125-preview
summary: "Elixir, z jego pot\u0119\u017Cnym dopasowywaniem wzorc\xF3w i wsparciem\
  \ dla \u0142\u0105czenia potokowego (pipelining), mo\u017Ce skutecznie obs\u0142\
  u\u017Cy\u0107 pliki CSV, nawet bez zewn\u0119trznych bibliotek."
title: Praca z plikami CSV
weight: 37
---

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
