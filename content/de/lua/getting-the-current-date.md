---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:24.945568-07:00
html_title:           "C: Aktuelles Datum abrufen"
simple_title:         "Aktuelles Datum abrufen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist das Lesen des gegenwärtigen Tages, Monats und Jahres vom System. Programmierer nutzen es, um zeitgesteuerte Funktionen, Protokollierungen oder Datumsstempel für Ereignisse zu realisieren.

## So geht's:
Du kannst das aktuelle Datum in Lua mit der `os.date()`-Funktion erhalten. Hier ein paar Beispiele:

```Lua
-- Vollständiges Datum und Uhrzeit
print(os.date())

-- Nur das aktuelle Jahr
print("Jahr: " .. os.date("%Y"))

-- Nur der aktuelle Monat
print("Monat: " .. os.date("%m"))

-- Nur der aktuelle Tag
print("Tag: " .. os.date("%d"))
```

Beispielausgabe:
```
Thu Mar 12 23:22:48 2023
Jahr: 2023
Monat: 03
Tag: 12
```

## Tiefere Einblicke:
`os.date()` gibt es schon eine Weile in Lua und lehnt sich an die C Standardbibliothek an. Alternativen umfassen das os.time() zur Ermittlung von Unix-Zeitstempeln und das nutzerdefinierte Parsing von `os.date()` für spezifische Formate. Die Implementierung verwendet die Systemzeit des Host-Rechners, was bedeutet, dass sie von den Systemeinstellungen für Zeitzone und Lokalisierung abhängt. Lua selbst bietet keine Zeitzonenberechnungen; dafür müssen externe Bibliotheken genutzt werden. 

## Siehe Auch:
- Lua-Handbuch: [https://www.lua.org/manual/5.4/](https://www.lua.org/manual/5.4/)
- os.date() Referenz: [https://www.lua.org/manual/5.4/manual.html#pdf-os.date](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- Lua Users Wiki zu Datums- und Zeitfunktionen: [http://lua-users.org/wiki/DateTime](http://lua-users.org/wiki/DateTime)