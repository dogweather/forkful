---
title:                "Arbeiten mit CSV-Dateien"
html_title:           "Ruby: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV und warum ist es wichtig?

CSV (Comma Separated Values) ist ein Dateiformat, das häufig verwendet wird, um Daten in Tabellenform zu speichern. Es besteht aus einer Auflistung von Zeilen und Spalten, wobei die Werte durch Kommas getrennt sind. Programmierer nutzen CSV, um große Datenmengen zu verwalten und zu analysieren, da es ein einfaches Format ist, das von vielen Programmen und Tools unterstützt wird.

## Wie geht's?

Ein Beispiel, um eine CSV-Datei zu lesen und Daten auszugeben: 

```Ruby
require 'csv'

CSV.foreach('data.csv') do |row|
  puts "Name: #{row[0]}, Alter: #{row[1]}, Stadt: #{row[2]}"
end
```
Ausgabe:
```
Name: Max, Alter: 25, Stadt: Berlin
Name: Lisa, Alter: 28, Stadt: Hamburg
Name: Tom, Alter: 30, Stadt: München
```

## Eintauchen

CSV wurde in den 1960er Jahren entwickelt, um den Austausch von Daten zwischen verschiedenen Computern zu vereinfachen. Heutzutage werden oft alternative Dateiformate wie JSON oder XML verwendet, aber CSV ist immer noch sehr verbreitet, da es einfach zu erstellen und zu lesen ist. In Ruby gibt es auch andere Methoden, um mit CSV-Daten zu arbeiten, wie beispielsweise die Klasse CSV::Table.

## Siehe auch

- [Ruby-Dokumentation zu CSV](https://ruby-doc.org/stdlib-2.7.0/libdoc/csv/rdoc/CSV.html)