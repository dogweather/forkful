---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Lua: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Berechnen eines Datums in der Zukunft oder Vergangenheit wird eine Zeitspanne zu einem spezifischen Datum hinzugefügt bzw. davon abgezogen. Programmeure machen dies, um historische Daten zu analysieren oder Ereignisse in der Zukunft vorauszuahnen.

## So geht's:

Hier ein einfaches Beispiel, wie man mith hilfe von Lua ein Datum in der Zukunft berechnet:

```Lua
-- Laden des os Moduls
local os = require("os")

-- Aktuelles Datum in Sekunden seit 1970
local jetzt = os.time()

-- Hinzufügen von 7 Tagen (in Sekunden gerechnet)
local in_einer_woche = jetzt + (7 * 24 * 60 * 60)

-- Umwandlung in lesbares Datum
local datum_in_einer_woche = os.date("%d.%m.%Y", in_einer_woche)

print("In einer Woche ist es: " .. datum_in_einer_woche)
```

Die Ausgabe könnte so aussehen:

```
In einer Woche ist es: 29.08.2023
```

## Tiefgang

Historisch gesehen war die Berechnung von Datumsangaben nicht Teil der frühen Programmierversionen. Dies wurde mit der Zeit eingeführt, da immer mehr Anwendungen eine Zeithandhabung benötigten. In Lua verlassen wir uns auf das os-Modul, das viele Funktionen zur Zeithandhabung bietet.

Es gibt auch andere Wege, um zukünftige oder vergangene Daten zu berechnen. Einige Programmiersprachen haben eingebaute Funktionen für diese Berechnungen, während andere auf externe Bibliotheken angewiesen sind. Eine Alternative in Lua könnte die Nutzung der os.difftime Funktion sein.

Bei der Implementierung ist zu beachten, dass Lua die Zeit in Sekunden seit dem 01.01.1970 berechnet. Addieren oder Subtrahieren wir eine Zeitspanne, tun wir das daher in Sekunden.

## Siehe auch

Für mehr Informationen und weiterführende Themen ans Herz gelegt:

- Die offizielle Lua-Dokumentation zum os-Modul: [https://www.lua.org/manual/5.3/manual.html#6.9](https://www.lua.org/manual/5.3/manual.html#6.9)
- Eine umfassende Erklärung zu Datum und Zeit in Lua: [http://lua-users.org/wiki/DateAndTime](http://lua-users.org/wiki/DateAndTime)

Dranbleiben und weitercoden! Du machst das schon klasse.