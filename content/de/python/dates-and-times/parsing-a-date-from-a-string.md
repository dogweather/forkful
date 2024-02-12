---
title:                "Einen Datum aus einem String analysieren"
aliases:
- de/python/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:52.591364-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen Datum aus einem String analysieren"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String beinhaltet das Umwandeln von textuellen Datum- und Zeitinformationen in ein datetime-Objekt oder ein äquivalentes strukturiertes Format. Dies wird häufig durchgeführt, um Datumsarithmetik, Vergleiche und Formatierungsoperationen auf eine Weise zu ermöglichen, die sprach- und regionsagnostisch ist. Programmierer tun dies, um temporal Daten, die aus Logs, Benutzereingaben oder externen Quellen extrahiert wurden, effizient zu handhaben und zu manipulieren.

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
