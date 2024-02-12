---
title:                "Arbeiten mit CSV"
aliases: - /de/elixir/working-with-csv.md
date:                  2024-02-03T19:19:34.227193-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Die Arbeit mit CSV-Dateien (Comma-Separated Values, dt. kommagetrennte Werte) umfasst das Lesen von und das Schreiben in diese Dateien, was eine häufige Notwendigkeit für Aufgaben ist, die Datenimport/-export oder einfache Speicherlösungen erfordern. Programmierer nutzen diese Funktionalität für den Datenaustausch zwischen Systemen, schnelles Dateneditieren oder für Situationen, in denen ein leichtgewichtiges und einfach zu manipulierendes Datenformat von Vorteil ist.

## Wie geht das:

Elixir kann mit seiner leistungsfähigen Mustererkennung und Unterstützung für Pipelining effizient CSV-Dateien verarbeiten, auch ohne externe Bibliotheken. Für fortgeschrittenere Bedürfnisse ist jedoch die Bibliothek `nimble_csv` eine schnelle und unkomplizierte Wahl.

### Eine CSV-Datei ohne externe Bibliotheken lesen

Sie können eine CSV-Datei lesen und parsen, indem Sie die integrierten Funktionen von Elixir verwenden:

```elixir
defmodule CSVReader do
  def read_file(dateipfad) do
    File.stream!(dateipfad)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# Beispielverwendung
CSVReader.read_file("data.csv")
# Ausgabe: [["Header1", "Header2"], ["Row1Value1", "Row1Value2"], ["Row2Value1", "Row2Value2"]]
```

### In eine CSV-Datei schreiben

Ähnlich, um Daten in eine CSV-Datei zu schreiben:

```elixir
defmodule CSVWriter do
  def write_to_file(dateipfad, daten) do
    File.open(dateipfad, [:write], fn datei ->
      Enum.each(daten, fn zeile ->
        IO.write(datei, Enum.join(zeile, ",") <> "\n")
      end)
    end)
  end
end

# Beispielverwendung
daten = [["Header1", "Header2"], ["Value1", "Value2"], ["Value3", "Value4"]]
CSVWriter.write_to_file("output.csv", daten)
# Erstellt output.csv mit den Daten im CSV-Format
```

### `nimble_csv` verwenden

Für komplexere CSV-Verarbeitungen bietet `nimble_csv` eine leistungsstarke und flexible Möglichkeit, mit CSV-Daten zu arbeiten. Fügen Sie zunächst `nimble_csv` zu Ihren Abhängigkeiten in `mix.exs` hinzu und führen Sie `mix deps.get` aus:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

CSV-Daten mit `nimble_csv` parsen:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(dateipfad) do
    dateipfad
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# Beispielverwendung
MyCSVParser.parse("data.csv")
# Die Ausgabe mit nimble_csv kann basierend auf der Definition angepasst werden, sieht aber im Allgemeinen aus wie eine Liste von Listen oder Tupeln, abhängig davon, wie Sie Ihren Parser einrichten.
```

Das Schreiben von CSV-Daten mit `nimble_csv` erfordert eine manuelle Umwandlung Ihrer Daten in ein geeignetes Format und anschließendes Schreiben in eine Datei, ähnlich wie im reinen Elixir-Beispiel, jedoch mit Nutzung von `nimble_csv` zum Generieren korrekt formatierter CSV-Zeilen.

Indem Sie den für die Komplexität Ihrer Aufgabe geeigneten Ansatz wählen, können Sie CSV-Dateien in Elixir mit großer Flexibilität und Kraft handhaben.
