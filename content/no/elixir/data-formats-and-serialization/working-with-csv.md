---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:14.264377-07:00
description: "Hvordan: Elixir, med sin kraftfulle m\xF8nstersammenligning og st\xF8\
  tte for r\xF8rlegging, kan h\xE5ndtere CSV-filer effektivt, selv uten tredjepartsbiblioteker.\
  \ Men\u2026"
lastmod: '2024-03-13T22:44:40.465682-06:00'
model: gpt-4-0125-preview
summary: "Elixir, med sin kraftfulle m\xF8nstersammenligning og st\xF8tte for r\xF8\
  rlegging, kan h\xE5ndtere CSV-filer effektivt, selv uten tredjepartsbiblioteker."
title: Arbeide med CSV
weight: 37
---

## Hvordan:
Elixir, med sin kraftfulle mønstersammenligning og støtte for rørlegging, kan håndtere CSV-filer effektivt, selv uten tredjepartsbiblioteker. Men for mer avanserte behov, er biblioteket `nimble_csv` et raskt og greit valg.

### Å lese en CSV-fil uten eksterne biblioteker
Du kan lese og tolke en CSV-fil ved å bruke Elixirs innebygde funksjoner:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Eksempel på bruk
CSVReader.read_file("data.csv")
# Utdata: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### Skrive til en CSV-fil
Tilsvarende, for å skrive data til en CSV-fil:

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

# Eksempel på bruk
data = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", data)
# Oppretter output.csv med dataene formatert som CSV
```

### Bruke `nimble_csv`
For mer kompleks håndtering av CSV, tilbyr `nimble_csv` en kraftfull og fleksibel måte å jobbe med CSV-data. Først, legg til `nimble_csv` i dine avhengigheter i `mix.exs` og kjør `mix deps.get`:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

Tolkning av CSV-data med `nimble_csv`:

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

# Eksempel på bruk
MyCSVParser.parse("data.csv")
# Utdata med nimble_csv kan tilpasses basert på definisjonen, men det ser generelt ut som en liste av lister eller tupler avhengig av hvordan du setter opp din tolker.
```

Å skrive CSV-data ved å bruke `nimble_csv` krever manuell transformasjon av dataene dine til et passende format og deretter skrive dem til en fil, mye likt det enkle Elixir-eksempelet, men utnytter `nimble_csv` for å generere korrekt formaterte CSV-rader.

Ved å velge den passende tilnærmingen for kompleksiteten av oppgaven din, kan du håndtere CSV-filer i Elixir med stor fleksibilitet og kraft.
