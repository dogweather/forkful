---
title:                "Umwandlung eines Datums in einen String"
html_title:           "Python: Umwandlung eines Datums in einen String"
simple_title:         "Umwandlung eines Datums in einen String"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Was ist das und warum?
Die Umwandlung eines Datums in einen String bedeutet, dass ein bestimmtes Datum (z. B. 10. Juni 2020) in eine lesbare Zeichenfolge (z. B. "10.06.2020") umgewandelt wird. Programmierer nutzen dies, um Daten in einem klareren Format anzeigen oder speichern zu können.

## So geht's:
```
Python from datetime import datetime
now = datetime.now()
print(now.strftime("%d.%m.%Y"))

# Output: 10.06.2020
```

## Tiefentauchen:
Die Umwandlung von Datum in Zeichenfolge ist ein wichtiger Bestandteil der Datumsmanipulation, um Daten übersichtlicher darzustellen. Es gibt jedoch auch andere Möglichkeiten, wie zum Beispiel die Umwandlung von Zeichenfolgen in Datum und die Verwendung von Modulen wie "python-dateutil" für fortgeschrittenere Funktionen. Bei der Implementierung ist es wichtig zu beachten, dass das Formatieren von Datum in eine Zeichenfolge von regionalen Einstellungen abhängen kann.

## Siehe auch:
• [Python-Dokumentation zu Datetime](https://docs.python.org/3/library/datetime.html)
• [Python-Dokumentation zu python-dateutil](https://dateutil.readthedocs.io/en/stable/)
• [Stack Overflow zu Datumsformatierung](https://stackoverflow.com/questions/7716630/converting-python-date-format-to-mysql)