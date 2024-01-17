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

# Was & Warum?

Als Programmierer kommt man häufig in Situationen, in denen man Daten in CSV-Dateien verarbeiten oder exportieren muss. CSV steht für "Comma Separated Values" und ist ein weit verbreitetes Dateiformat für den Austausch von Tabellen oder Listen. Durch das Arbeiten mit CSV können Programmierer Daten einfach importieren und exportieren und sie in verschiedenen Anwendungen nutzen.

# Wie geht's?

Das Arbeiten mit CSV in Elixir ist dank des Moduls "CSV" sehr einfach. Wir können eine CSV-Datei mit der Funktion ```CSV.parse``` einlesen und sie mit ```CSV.encode``` wieder in das CSV-Format zurückkonvertieren. Ein Beispiel sieht wie folgt aus:

```Elixir
input = File.read!("data.csv")

# Einlesen der Datei
{:ok, data, _} = CSV.parse(input) 

# Ausgabe der Daten in der Konsole
data |> Enum.each(fn row -> IO.inspect row end) 

# Erstellen einer neuen CSV-Datei
output = CSV.encode(data)
File.write!("neue_data.csv", output)
```

Die Ausgabe wird etwa so aussehen:

```
["Name","Alter","Ort"]
["Max",29,"Berlin"]
["Anna",25,"Hamburg"]
["Tom",32,"München"]
```

# Tiefere Einblicke

CSV ist ein relativ einfaches Dateiformat, das seit den 1970er Jahren existiert. Es wurde entwickelt, um Daten zwischen verschiedenen Anwendungen auszutauschen, da es plattformunabhängig ist und von vielen Programmen unterstützt wird.

In Elixir gibt es auch andere Möglichkeiten, mit CSV zu arbeiten, wie zum Beispiel das "ErlCSV" Paket, das komplexere Funktionen zum Lesen und Schreiben von CSV-Dateien bietet.

Die Implementierung von CSV in Elixir ist sehr effizient, da Elixir auf der Erlang Virtual Machine (VM) basiert, die sich insbesondere für die gleichzeitige Verarbeitung von Daten eignet.

# Siehe auch

- Offizielle Dokumentation zu "CSV" Modul in Elixir: https://hexdocs.pm/csv/CSV.html
- "ErlCSV" Paket: https://github.com/vishnevskiy/Erlang-CSV