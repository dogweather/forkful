---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

In Lua können wir das aktuelle Datum abrufen, das oft zur Registrierung von Zeitstempeln, zur Berechnung der verstrichenen Zeit oder zur Planung zukünftiger Ereignisse eingesetzt wird.

## Wie tut man das:

Hier sind einige Möglichkeiten, wie Sie das aktuelle Datum in Lua abrufen können.

```lua
os.date() -- Gibt das aktuelle Datum und die Uhrzeit im Format "Tue Feb 14 02:48:10 2012" aus.

os.date("%x") -- Gibt das aktuelle Datum im lokalen Datumsformat aus. Zum Beispiel "02/14/12".

os.date("%Y-%m-%d") -- Gibt das aktuelle Datum im Format "2012-02-14" aus.
```

## Deep Dive

Lua, in der ersten Übersetzung im Jahr 1993 veröffentlicht, hat eine `os.date` Funktion zum Abrufen des aktuellen Datums. Diese Funktion gibt eine formatierte Zeichenkette zurück, die das Datum und die Uhrzeit repräsentiert. Default formatiert es nach C's `strftime`. 

Es gibt alternativen zu `os.date` wie die `os.time` Funktion, welche die Anzahl der Sekunden seit 1970 (the Unix time) liefert. Diese kann dann benutzt werden, um das aktuelle Datum zu bestimmen.

In Bezug auf die Implementierung, nutzt die Funktion `os.date` die C Funktion `strftime` um das Datum und die Uhrzeit in einer Zeichenkette darzustellen. Es bedient sich dabei verschiedener Direktiven, um verschiedene Formate zu erzeugen.

## Siehe auch

Die Lua-Dokumentation für die `os.date` Funktion: http://www.lua.org/manual/5.4/manual.html#6.9

StackOverflow-Thread über die Datumsformatierung in Lua: https://stackoverflow.com/questions/36054930/how-to-format-date-in-lua

C's strftime Funktion: https://www.cplusplus.com/reference/ctime/strftime/