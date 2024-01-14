---
title:                "Elixir: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

Warum sollte man sich mit der CSV-Verarbeitung in Elixir beschäftigen?

Wenn du jemals mit Tabellenkalkulationsdateien wie Excel oder Google Sheets gearbeitet hast, bist du vielleicht schon auf das CSV-Format gestoßen. CSV steht für "Comma Separated Values" und ist eine übersichtliche Möglichkeit, Daten in Textform zu speichern und auszutauschen. In diesem Blogbeitrag werden wir uns ansehen, wie du mit CSV-Dateien in Elixir arbeiten kannst.

Wie man mit CSV-Dateien in Elixir arbeitet

Zunächst einmal müssen wir die CSV-Bibliothek in unserem Elixir-Projekt hinzufügen. Wir können dies tun, indem wir die folgende Zeile in unsere `mix.exs`-Datei einfügen:

```Elixir
defp deps do
  [
    {:csv, "~> 2.2"}
  ]
end
```

Als nächstes müssen wir die `csv`-Funktion importieren, um sie in unserem Code verwenden zu können:

```Elixir
import CSV
```

Nun können wir eine CSV-Datei in unserem Code öffnen und Daten daraus lesen. Hier ist ein Beispiel, wie wir eine CSV-Datei mit einer Überschriftszeile öffnen und die Daten in eine Liste von Maps lesen können:

```Elixir
contents = File.read!("example.csv")
headers = :line.split(headers, ",") |> List.to_tuple()
data = :line.split(contents, "\n") |> Enum.drop(1) |> Enum.map(fn line ->
          :line.split(line, ",") |> List.to_tuple()
        end) |> Enum.map(fn entry ->
          Map.new(headers, entry)
        end)
```

Die Variable `data` wird nun eine Liste von Maps sein, wobei jede Map eine Zeile der CSV-Datei darstellt und die Spalten der CSV-Datei als Schlüssel und die entsprechenden Werte als Werte enthält.

Wir können auch Daten aus unserer Anwendung in eine CSV-Datei schreiben. Hier ist ein Beispiel, wie wir eine Liste von Maps in eine CSV-Datei mit den entsprechenden Spaltennamen schreiben können:

```Elixir
data = [
  %{id: 1, name: "John", age: 25},
  %{id: 2, name: "Kate", age: 30},
  %{id: 3, name: "Mike", age: 35}
]
columns = [:id, :name, :age]
CSV.encode(columns, data, encoding: :unicode)
|> File.write!("example.csv")
```

Die Datei `example.csv` wird nun folgenden Inhalt haben:

```
id,name,age
1,John,25
2,Kate,30
3,Mike,35
```

Tiefergehende Informationen zur CSV-Verarbeitung

In diesem kurzen Tutorial haben wir nur die Grundlagen der CSV-Verarbeitung in Elixir behandelt. Es gibt noch viel mehr, was du mit CSV-Dateien in Elixir tun kannst, wie zum Beispiel das Filtern und Sortieren von Daten. Du kannst auch die `CSV`-Funktionen anpassen, um dein eigenes CSV-Format zu erstellen oder zu verarbeiten.

Wenn du mehr über die `CSV`-Bibliothek erfahren möchtest, kannst du ihre offizielle Dokumentation unter https://hexdocs.pm/csv/2.2.0/ lesen.

Siehe auch

- Offizielle Dokumentation für die `CSV`-Bibliothek: https://hexdocs.pm/csv/2.2.0/
- Elixir String API: https://hexdocs.pm/elixir/String.html
- Elixir Enum API: https://hexdocs.pm/elixir/Enum.html