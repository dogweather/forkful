---
title:                "Arbeiten mit CSV"
html_title:           "Lua: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-csv.md"
---

{{< edit_this_page >}}

## Was ist CSV?
CSV steht für Comma-Separated Values, was übersetzt Komma-getrennte Werte bedeutet. Es ist ein Dateiformat zur Speicherung von tabellarischen Daten in Textform, bei dem die Datenfelder durch Kommas getrennt sind. CSV ist besonders nützlich für Programmierer, da es eine einfache und flexible Möglichkeit bietet, Daten zu speichern und zu verwenden.

## Warum nutzen Programmierer CSV?
CSV ist ein sehr beliebtes Format, da es plattformübergreifend unterstützt wird und von vielen Programmen, Datenbanken und Anwendungen gelesen und geschrieben werden kann. Es ist auch sehr einfach zu erstellen und zu bearbeiten, da CSV-Dateien einfach mit einem Texteditor geöffnet werden können.

## Wie geht's?
Um mit CSV in Lua zu arbeiten, benötigen wir die eingebaute Bibliothek "csv", die uns die Funktionen zum Lesen und Schreiben von CSV-Dateien zur Verfügung stellt. Hier ist ein Beispielcode, wie eine CSV-Datei erstellt und gelesen werden kann:

```Lua
-- CSV-Datei erstellen
csv = require("csv")
csvfile = csv.open("meine_daten.csv", "w") -- Datei im Schreibmodus öffnen

-- Datenfelder in die Datei schreiben
csvfile:write({"Name", "Alter", "Stadt"}) -- Überschriften
csvfile:write({"Max", 25, "Berlin"}) -- Datenzeile 1
csvfile:write({"Anna", 30, "Hamburg"}) -- Datenzeile 2
csvfile:close() -- Datei schließen

-- CSV-Datei lesen
csvfile = csv.open("meine_daten.csv", "r") -- Datei im Lesemodus öffnen
for fields in csvfile:lines() do -- Schleife über alle Datenzeilen in der Datei
  print(fields[1], fields[2], fields[3]) -- Datenfelder ausgeben
end
csvfile:close() -- Datei schließen
```

Die Ausgabe dieses Codes wäre:

```
Name Alter Stadt
Max 25 Berlin
Anna 30 Hamburg
```

## Tiefere Einblicke
CSV wurde in den 1970er Jahren bei IBM entwickelt und ist seitdem ein weit verbreitetes Format. Es gibt auch verschiedene Alternativen zu CSV wie z.B. JSON oder XML, die jedoch häufig komplexer zu lesen und zu erstellen sind.

Die Implementierung der "csv" Bibliothek in Lua folgt dem RFC 4180 Standard, der die Formatierung von CSV-Dateien festlegt. Die Bibliothek unterstützt auch Optionen wie z.B. andere Trennzeichen als Kommas oder das Überspringen von Überschriften.

## Siehe auch
- [Offizielle Lua CSV Bibliothek Dokumentation](https://github.com/geoffleyland/lua-csv/wiki)
- [CSV Wikipedia Artikel auf Deutsch](https://de.wikipedia.org/wiki/CSV_(Dateiformat))
- [RFC 4180 Spezifikation für CSV Format](https://datatracker.ietf.org/doc/html/rfc4180)