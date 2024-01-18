---
title:                "Erstellen einer temporären Datei."
html_title:           "Lua: Erstellen einer temporären Datei."
simple_title:         "Erstellen einer temporären Datei."
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erstellen einer temporären Datei ist eine nützliche Technik, die Programmierer verwenden, um kurzzeitig Daten zu speichern. Dies kann hilfreich sein, wenn bestimmte Funktionen oder Vorgänge während der Programmausführung ausgeführt werden müssen. Durch die Verwendung von temporären Dateien können Programmierer sicherstellen, dass ihre Daten vorübergehend gespeichert werden, ohne die Codebasis zu beeinflussen.

## Wie geht's?
Um eine temporäre Datei in Lua zu erstellen, können wir die Funktion `os.tmpname()` verwenden. Diese Funktion generiert einen zufälligen Dateinamen und speichert ihn im temporären Ordner des Betriebssystems. Wir können diesen Dateinamen dann verwenden, um unsere temporäre Datei zu erstellen und zu verwenden, wie im folgenden Beispiel gezeigt:

```Lua
local temp_file_name = os.tmpname()
-- Hier haben wir einen zufälligen Dateinamen erhalten, z.B.: "C:\Users\Temp\luatemp2491dgfktmp"
local temp_file = io.open(temp_file_name, "w")
temp_file:write("Dies ist ein Beispielinhalt für unsere temporäre Datei.")
temp_file:close()
```

Wir haben jetzt eine temporäre Datei namens `temp_file`, die im Betriebssystem-Temp-Verzeichnis gespeichert ist und den Inhalt "Dies ist ein Beispielinhalt für unsere temporäre Datei." enthält. Wir können diese Datei dann wie jede andere Datei in unserem Programm verwenden.

## Tiefere Einblicke
Das Erstellen von temporären Dateien ist seit langem eine gängige Technik in der Programmierung, insbesondere in Betriebssystemen wie Unix, wo es häufig zum Speichern von temporären Dateien verwendet wird. In Lua gibt es zwar andere Möglichkeiten, temporäre Daten zu speichern, wie z.B. in Tabellen oder Variablen, jedoch können temporäre Dateien nützlich sein, um größere Datenmengen vorübergehend zu speichern.

Alternativ können Programmierer auch die `io.tmpfile()` Funktion verwenden, um eine temporäre Datei zu erstellen, die sich im Arbeitsspeicher befindet, anstatt im Dateisystem. Dies kann nützlich sein, wenn der Zugriff auf eine Datei im Dateisystem zu langsam ist.

Die `os.tmpname()` Funktion ist seit Lua 5.2 standardmäßig verfügbar, während `io.tmpfile()` seit Lua 5.1 verfügbar ist.

## Siehe auch
* [Lua 5.4 Referenzhandbuch: os.tmpname()](https://www.lua.org/manual/5.4/manual.html#6.9)
* [Lua 5.4 Referenzhandbuch: io.tmpfile()](https://www.lua.org/manual/5.4/manual.html#6.10)