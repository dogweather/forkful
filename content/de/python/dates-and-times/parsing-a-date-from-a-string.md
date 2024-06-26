---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:52.591364-07:00
description: "Wie geht das: Die Standardbibliothek von Python stellt das `datetime`\
  \ Modul bereit, welches die `strptime` Methode f\xFCr diesen Zweck beinhaltet. Die\u2026"
lastmod: '2024-03-13T22:44:53.388320-06:00'
model: gpt-4-0125-preview
summary: "Die Standardbibliothek von Python stellt das `datetime` Modul bereit, welches\
  \ die `strptime` Methode f\xFCr diesen Zweck beinhaltet."
title: Einen Datum aus einem String analysieren
weight: 30
---

## Wie geht das:
Die Standardbibliothek von Python stellt das `datetime` Modul bereit, welches die `strptime` Methode für diesen Zweck beinhaltet. Die Methode benötigt zwei Argumente: den Datum-String und eine Formatdirektive, die das Muster des Eingabestrings angibt.

```python
from datetime import datetime

# Beispielstring
date_string = "2023-04-01 14:30:00"
# Parsen des Strings zu einem datetime-Objekt
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Ausgabe: 2023-04-01 14:30:00
```

Für nuancierteres Datumparsen, besonders beim Umgang mit mehreren Formaten oder Lokalen, kann die Drittanbieter-Bibliothek `dateutil` extrem hilfreich sein. Sie liefert ein Parser-Modul, das Daten in fast jedem Stringformat parsen kann.

```python
from dateutil import parser

# Beispielstrings
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1. April 2023 14:30"

# Verwendung des Parsers von dateutil
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Ausgabe: 2023-04-01 14:30:00
print(parsed_date2)
# Ausgabe: 2023-04-01 14:30:00
```

`dateutil` ist geschickt im Umgang mit den meisten Datumsformaten ohne explizite Formatstrings, was es zu einer vielseitigen Wahl für Anwendungen macht, die mit diversen Datumsdarstellungen arbeiten.
