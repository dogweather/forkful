---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV (Comma-Separated Values) ist ein einfaches Format, um tabellarische Daten zu speichern und zu transportieren. Programmierer nutzen es, weil es einfach zu lesen, zu schreiben und von den meisten Datenverarbeitungstools unterstützt wird.

## Anleitung:
Mit Elixir können Sie CSV-Dateien leicht bearbeiten. Hier ein Beispiel zum Einlesen und Schreiben.

Einlesen:

```elixir
defmodule CSVExample do
  require CSV

  def read_csv(file_path) do
    file_path
    |> File.stream!()
    |> CSV.decode(separator: ?;)
    |> Enum.to_list()
  end
end

# Benutzung:
rows = CSVExample.read_csv("meine_daten.csv")
IO.inspect(rows)
```

Schreiben:

```elixir
defmodule CSVExample do
  require CSV

  def write_csv(file_path, data) do
    CSV.encode_to_iodata(data, separator: ?;)
    |> Enum.each(fn row -> File.write!(file_path, row) end)
  end
end

# Benutzung:
data = [["Name", "Alter"], ["Max", 28], ["Anna", 22]]
CSVExample.write_csv("meine_daten.csv", data)
```

## Tiefere Einblicke:
CSV ist seit den frühen Computerjahren in Gebrauch und bleibt wegen seiner Simplizität und Breiten Kompatibilität populär. Alternativen wie JSON oder XML bieten strukturiertere Datenformate, können aber überwältigend sein, wenn Sie nur einfache tabellarische Daten benötigen. In Elixir wird oft `CSV` (eine Community-bibliothek) für das Arbeiten mit CSV-Dateien verwendet, weil es performant ist und gut mit Elixir's funktionalen Features zusammenpasst.

## Siehe auch:
- Elixir CSV Library auf Hex: [https://hex.pm/packages/csv](https://hex.pm/packages/csv)
- Offizielle Elixir Dokumentation: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
