---
title:                "Das Abrufen des aktuellen Datums"
html_title:           "Python: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine Möglichkeit für Programmierer, die aktuelle Zeit zu ermitteln. Dies kann zum Beispiel für die Verwaltung von Dateien oder das Protokollieren von Ereignissen in einer Anwendung nützlich sein.
## Wie geht's?
```Python
import datetime

current_date = datetime.date.today()
current_time = datetime.datetime.now()

print(current_date)
print(current_time)
```
#### Ausgabe:
```
2021-09-07
2021-09-07 12:34:56.789012
```
## Tiefere Einblicke
Die Python-Dokumentation bietet eine umfassende Erklärung zur Verwendung des datetime-Moduls für die Arbeit mit Datum und Uhrzeit. Es gibt auch alternative Module wie 'time' und 'calendar', die für bestimmte Anwendungsfälle geeignet sein können. Bei der Arbeit mit Datum und Uhrzeit ist es auch wichtig, die Zeitzone und die möglichen Auswirkungen von Sommerzeitänderungen zu berücksichtigen.
## Siehe auch
- [Python-Dokumentation zu datetime] (https://docs.python.org/3/library/datetime.html)
- [Python-Dokumentation zu time] (https://docs.python.org/3/library/time.html)
- [Python-Dokumentation zu calendar] (https://docs.python.org/3/library/calendar.html)