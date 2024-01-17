---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Python: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Berechnung eines Datums in der Zukunft oder Vergangenheit ist ein häufiger Programmiertask, bei dem ein Programmierer ein bestimmtes Datum jenseits des aktuellen Datums berechnen muss. Dies kann nützlich sein, um beispielsweise wiederkehrende Ereignisse zu planen oder um mit verschiedenen Zeitzonen umzugehen.

## Wie geht's?
```Python
# Importieren des datetime-Moduls
import datetime

# Berechnung des Datums in der Zukunft
future_date = datetime.datetime.now() + datetime.timedelta(days=7)
print("Das Datum in 7 Tagen wird sein:", future_date.date())

# Berechnung des Datums in der Vergangenheit
past_date = datetime.datetime.now() - datetime.timedelta(days=14)
print("Das Datum vor 14 Tagen war:", past_date.date())
```

Output:
Das Datum in 7 Tagen wird sein: 2021-09-29
Das Datum vor 14 Tagen war: 2021-09-09

## Tiefgehende Informationen
Die Berechnung von Datumswerten in der Zukunft oder Vergangenheit wurde durch die Notwendigkeit geschaffen, mit wechselnden Kalendern und Zeitzonen umzugehen. Es gibt jedoch auch alternative Methoden, wie die Verwendung von Libraries wie "arrow" oder "dateutil". Bei der Implementierung ist es wichtig, das richtige Zeitformat zu verwenden und mit der Zeitzone korrekt umzugehen.

## Siehe auch
- [Python datetime-Modul Dokumentation](https://docs.python.org/3/library/datetime.html)
- [Python arrow-Modul](https://arrow.readthedocs.io/en/latest/index.html)
- [Python dateutil-Modul](https://dateutil.readthedocs.io/en/stable/index.html)