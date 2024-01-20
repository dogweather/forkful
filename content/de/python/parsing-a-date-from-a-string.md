---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:37:50.121640-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Beim Parsen eines Datums aus einem String wird Text in ein Datum-Objekt umgewandelt. Programmierer machen das, um Daten aus Textdateien, Benutzereingaben oder Datenbanken zu extrahieren und damit zu arbeiten.

## How to (Wie man es macht):
Um ein Datum aus einem String in Python zu parsen, nutzen wir die `datetime` Bibliothek. Sie hat die `strptime`-Methode, die Strings in Datumsobjekte umwandelt. Hier ist ein einfaches Beispiel:

```Python
from datetime import datetime

datum_string = "2023-04-12"
datum_objekt = datetime.strptime(datum_string, "%Y-%m-%d")

print(datum_objekt)  # Ausgabe: 2023-04-12 00:00:00
```

Das Format `%Y-%m-%d` steht für Jahr-Monat-Tag und muss dem Format des Eingabestrings entsprechen.

## Deep Dive (Tiefergehende Betrachtung):
Das Parsen von Daten aus Strings ist eine alte Praxis, die auf die Anfänge der Programmierung zurückgeht, als Daten noch als Text übertragen wurden. Python's `datetime.strptime` ist ein starkes Werkzeug, erlaubt aber keine Fehler im Datumsformat. Bist du nicht sicher über das Format, musst du entweder sämtliche Möglichkeiten vorsehen oder Bibliotheken wie `dateutil.parser` nutzen, die flexiblere Parsing-Optionen bieten.

Python's Standardmethode des Datumparsens über `strptime` ist effizient, aber manchmal braucht es einen erleichterten Umgang mit Formaten, Zeitzonen oder Lokalitäten. `dateutil.parser` kann zum Beispiel besser mit unterschiedlichen Sprachen und Datumsformaten umgehen und erkennt oft das richtige Format automatisch.

```Python
from dateutil import parser

datum_string = "12. April 2023"
datum_objekt = parser.parse(datum_string)

print(datum_objekt)  # Ausgabe: 2023-04-12 00:00:00
```

Das Ausgabedatum ist dabei das gleiche Objekt wie zuvor, aber der Input konnte freier in natürlicher Sprache angegeben werden.

## See Also (Siehe auch):
- Python's `datetime` Dokumentation: https://docs.python.org/3/library/datetime.html
- `dateutil.parser` Dokumentation: https://dateutil.readthedocs.io/en/stable/parser.html
- ISO 8601 Datum und Zeit Standards: https://www.iso.org/iso-8601-date-and-time-format.html