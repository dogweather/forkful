---
title:                "Arbeiten mit csv"
html_title:           "Elixir: Arbeiten mit csv"
simple_title:         "Arbeiten mit csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Warum

CSV-Dateien (Comma-Separated Values) sind eine der häufigsten Dateiformate, die in der Programmierung verwendet werden. Sie sind einfach zu erstellen und zu lesen, und können Daten in einer tabellarischen Form organisieren. Daher ist es wichtig, zu verstehen, wie man mit CSV-Dateien in Elixir arbeiten kann.

## Wie geht's?

Das Erlernen des Umgangs mit CSV in Elixir ist relativ einfach und erfordert nur wenige Schritte. Zunächst müssen wir das `CSV`-Modul importieren, indem wir `require CSV` in unser Elixir-Skript oder in die Elixir-Eingabeaufforderung eingeben. Anschließend können wir die Funktion `CSV.parse` verwenden, um eine CSV-Datei zu lesen und sie in eine Liste von Listen zu konvertieren.

```Elixir
require CSV

data = CSV.parse("data.csv")
IO.inspect data

# Output:
[["Name", "Alter"],
 ["Maria", "35"],
 ["Alex", "42"],
 ["Julia", "27"]]
```

Wie Sie sehen können, wird die CSV-Datei in eine Liste von Listen konvertiert, wobei jede Zeile der Datei als separate Liste behandelt wird. Um auf die Daten zuzugreifen, können wir einfach auf die entsprechenden Elemente in der Liste zugreifen.

```Elixir
# Zugriff auf den Namen der ersten Person
IO.puts(data[1][0]) # Output: Maria

# Zugriff auf das Alter der dritten Person
IO.puts(data[3][1]) # Output: 27
```

Wenn wir Änderungen an der CSV-Datei vornehmen oder eine neue CSV-Datei erstellen möchten, können wir die Funktion `CSV.encode` verwenden, um eine Liste von Listen in eine CSV-Datei zu konvertieren.

```elixir
# Erstellen einer neuen CSV-Datei
new_data = [["Name", "Beruf"],
            ["Lisa", "Ingenieur"],
            ["Tom", "Lehrer"]]

CSV.encode("new_data.csv", new_data)
```

## Tiefer in die Materie

Es gibt viele Optionen, die wir beim Lesen und Schreiben von CSV-Dateien in Elixir nutzen können. In der `CSV.parse`-Funktion können wir zum Beispiel verschiedene Parameter wie `headers` oder `converters` angeben, um die Daten in gewünschter Form zu erhalten. Ebenso können wir in der `CSV.encode`-Funktion zusätzliche Optionen wie `col_sep` oder `quote_char` angeben, um das Format der CSV-Datei anzupassen.

Außerdem gibt es noch das `Ecto.Adapters.CSV`-Modul, das speziell für die Arbeit mit CSV-Dateien in Kombination mit der Datenbank-Abstraktionsschicht Ecto entwickelt wurde. Dieses Modul ermöglicht es uns unter anderem, CSV-Daten direkt in eine Datenbank zu importieren oder sie aus einer Datenbank zu exportieren.

## Siehe auch

- Offizielle Elixir CSV-Dokumentation: https://hexdocs.pm/elixir/CSV.html
- Ecto.Adapters.CSV-Modul: https://hexdocs.pm/ecto/Ecto.Adapters.CSV.html
- Elixir School Tutorial zum Arbeiten mit CSV in Elixir: https://elixirschool.com/de/lessons/advanced/csv/