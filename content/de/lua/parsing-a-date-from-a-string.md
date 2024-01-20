---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String ist der Prozess, einen Datumsstring in eine formatierte Datumsstruktur umzuwandeln. Programmierer machen dies, um Daten zu sortieren, zu filtern und einfacher mit ihnen zu arbeiten.

## Wie geht das?

In Lua wird das Parse-Funktion in der `os.date` Funktion gefunden. Hier ist ein einfaches Beispiel:

```Lua
datumstring = "20/04/2022"
tag, monat, jahr = datumstring:match("([^/]+)/([^/]+)/([^/]+)")

parsed_datum = os.date("*t", os.time{year=jahr, month=monat, day=tag})

print(parsed_datum.day, parsed_datum.month, parsed_datum.year) 
```

Wenn Sie das obige Programm ausführen, wird die Ausgabe wie folgt angezeigt:

```Lua
20  4  2022
```

## Tauchen wir tiefer ein

Die `os.date` Funktion in Lua geht auf die Unix-Ära zurück, als die Umwandlung von Zeichenketten in Daten eine häufige Aufgabe war. Es gibt auch Alternativen wie das `date-parser` Modul in der LuaRocks Bibliothek, das erweiterte Optionen bietet. Die Implementierungsdetails des `os.date` beinhalten die Verwendung von Musterabgleich in Lua, um die verschiedenen Teile des Datums aus dem String zu extrahieren.

## Siehe auch

1. Lua String Library Dokumentation: [https://www.lua.org/manual/5.3/manual.html#6.4](https://www.lua.org/manual/5.3/manual.html#6.4)
3. Lua OS Library Dokumentation: [https://www.lua.org/manual/5.4/manual.html#6.9](https://www.lua.org/manual/5.4/manual.html#6.9)