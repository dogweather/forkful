---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:15:59.907284-07:00
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Programmierer holen das aktuelle Datum, um zu wissen, welches Datum heute ist, und verwenden es in Anwendungen, wie zum Beispiel bei Logs, Zeitstempeln oder periodischen Aufgaben. Es ist ein grundlegendes Feature, das in fast jeder Software benötigt wird.

## How to:

Python macht es super einfach, das aktuelle Datum zu bekommen. Hier ist, wie's geht:

```Python
from datetime import date

heute = date.today()
print("Heute ist:", heute)
```

Ausgabe könnte so aussehen:

```
Heute ist: 2023-04-05
```

Du möchtest Zeit auch dazu? Kein Problem:

```Python
from datetime import datetime

jetzt = datetime.now()
print("Jetzt ist:", jetzt.strftime("%Y-%m-%d %H:%M:%S"))
```

Und sieht dann so aus:

```
Jetzt ist: 2023-04-05 15:22:31
```

## Deep Dive

Das Modul `datetime` gibt es in Python schon eine ganze Weile. Es ermöglicht mehr als nur das aktuelle Datum zu bekommen – Formatierung, Zeitzonen und komplexe Berechnungen sind drin. Alternativen? Naja, für einfache Fälle reicht eigentlich `datetime`. Könntest ältere Module wie `time` verwenden, aber warum kompliziert, wenn es auch einfach geht?

Implementierungs-Details? Das `datetime` Modul basiert auf C-Funktionen. Es ist effizient, aber man sollte sich mit den Timezones auskennen, sonst gibt's Durcheinander. Übrigens, `date.today()` gibt dir das Datum in der lokalen Zeitzone deines Computers. 

## See Also

Ihr wollt tiefer einsteigen oder braucht spezifische Lösungen? Hier gibts mehr Infos:

- Offizielle Python-Dokumentation zum `datetime` Modul: https://docs.python.org/3/library/datetime.html
- Mehr über Zeit und Datumsmanipulation: https://dateutil.readthedocs.io/en/stable/
- Und ein cooles Tutorial zu `datetime`: https://realpython.com/python-datetime/
