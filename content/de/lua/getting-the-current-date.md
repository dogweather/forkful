---
title:                "Das heutige Datum erhalten."
html_title:           "Lua: Das heutige Datum erhalten."
simple_title:         "Das heutige Datum erhalten."
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist ein häufig verwendetes Programmierkonzept, bei dem der aktuelle Kalendertag und die Uhrzeit abgerufen werden. Programmierer nutzen dies, um Zeitstempel für ihre Dateien zu erstellen, oder um den Nutzern aktuelle Informationen anzuzeigen.

## Wie geht's:
Um das aktuelle Datum in Lua zu bekommen, können wir die Funktion ```os.date()``` verwenden. Hier ist ein Beispielcode mit der Ausgabe des aktuellen Datums und der Uhrzeit:

```Lua
local date = os.date("%x %X")
print(date)
```

Die Ausgabe für dieses Beispiel könnte etwas Ähnliches wie "12/14/21 10:30:45" sein. Die Funktion ```os.date()``` akzeptiert ein Formatierungsmuster als Argument, um das Datum und die Uhrzeit in verschiedenen Formaten auszugeben. In diesem Beispiel wird ```%x``` für das Datum im Format "MM/TT/JJ" und ```%X``` für die Uhrzeit im Format "HH:MM:SS" verwendet.

## Tiefentauchen:
Das Abrufen des aktuellen Datums ist ein wichtiges Konzept, da es in vielen Anwendungsbereichen wie Datenbanken, Dateien und Benutzeroberflächen verwendet wird. Alternativ könnten wir auch die Funktion ```os.time()``` verwenden, die die Anzahl der Sekunden seit dem 1. Januar 1970 zurückgibt. Diese Anzahl kann in verschiedene Formate für Datum und Uhrzeit umgewandelt werden.

Um genaue Zeitangaben zu erhalten, wird empfohlen, zuerst die Funktion ```os.time()``` aufzurufen und dann die Funktion ```os.date()``` mit dem Rückgabewert als Argument zu verwenden.

## Siehe auch:
- [Lua Dokumentation: io.date()](https://www.lua.org/manual/5.1/manual.html#pdf-io.date)
- [Lua Programmierkurs: Zeit und Datum](https://www.lua.org/pil/22.1.html)