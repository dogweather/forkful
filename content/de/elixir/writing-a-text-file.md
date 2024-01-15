---
title:                "Das Schreiben einer Textdatei"
html_title:           "Elixir: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind ein grundlegender Bestandteil der Programmierung. Sie dienen dazu, Daten oder Code auf eine lesbare und organisierte Weise zu speichern und können in allen gängigen Programmiersprachen verwendet werden. In diesem Artikel werden wir uns ansehen, wie wir in Elixir Textdateien erstellen und bearbeiten können.

## Wie geht das?

Um eine Textdatei in Elixir zu erstellen, müssen wir zuerst eine Datei öffnen und einen Dateipfad angeben. Hier ist ein Beispiel für eine einfache Funktion, die eine Datei mit dem Namen "hello.txt" erstellt:

```Elixir
def create_text_file do
  file_path = "hello.txt"
  File.open(file_path, [:write, :utf8])    # Öffnet die Datei im Schreibmodus und stellt sicher, dass sie als utf8 formatiert ist.
  |> IO.puts("Hallo Leute!")              # Schreibt den Text "Hallo Leute!" in die Datei.
  |> IO.close                             # Schließt die Datei.
end
```

Um eine bestehende Datei zu bearbeiten, können wir die `File.stream!/2` Funktion verwenden, die eine Datei in einen Datenstrom umwandelt und uns ermöglicht, Zeilen zu lesen, hinzuzufügen oder zu entfernen. Hier ist ein Beispiel, das eine vorhandene Textdatei ("hello.txt") liest und den Text "Guten Morgen!" am Ende hinzufügt:

```Elixir
def edit_text_file do
  "hello.txt"
  |> File.stream!                           # Wandelt die Datei in einen Datenstrom um.
  |> Enum.map(&IO.chardata_to_string/1)     # Konvertiert jede Zeile in einen lesbaren String.
  |> Enum.to_list                           # Wandelt den Strom in eine Liste um, damit wir eine Zeile hinzufügen können.
  |> Kernel.++(["Guten Morgen!"])           # Fügt den Text am Ende der Liste hinzu.
  |> File.write("hello.txt")                # Schreibt die aktualisierte Liste zurück in die Datei.
end
```

## Tief eintauchen

In Elixir gibt es mehrere Bibliotheken, die uns beim Lesen und Schreiben von Textdateien unterstützen. Eine davon ist die `CSV`-Bibliothek, die uns ermöglicht, CSV-Dateien zu lesen, zu erstellen und zu bearbeiten. Hier ist ein Beispiel, wie wir eine CSV-Datei mit dem Inhalt eines Datenstroms erstellen können:

```Elixir
def create_csv_datastream(data) do
  "data.csv"
  |> CSV.encode(data)                       # Wandelt die Daten in ein csv-kompatibles Datenformat um.
  |> File.write("data.csv")                 # Schreibt den Datenstrom in eine Datei mit dem angegebenen Namen.
end
```

Es gibt auch die `Poison`-Bibliothek, die uns beim Lesen und Schreiben von JSON-Dateien unterstützt. Hier ist ein Beispiel, wie wir eine JSON-Datei lesen und in ein Elixir-Datenformat umwandeln können:

```Elixir
def convert_json_to_elixir do
  json = File.read("data.json")
  |> Poison.decode                          # Decodiert den JSON-String und gibt ein Elixir-Datenformat zurück.
end
```

## Siehe auch

- Offizielle Elixir-Dokumentation zu Dateisystemen: https://hexdocs.pm/elixir/File.html
- Die `CSV`-Bibliothek: https://hexdocs.pm/csv/CSV.html
- Die `Poison`-Bibliothek: https://github.com/devinus/poison