---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein Datum aus einem Zeichenfolge zu parsen bedeutet, die Zeichenfolge zu analysieren und das darin vorhandene Datum zu extrahieren. Dies wird oft von Programmierern benötigt, wenn sie mit Daten arbeiten, die in Textform (wie in einer CSV-Datei) vorliegen.

## Wie man:

Hier ist ein Codebeispiel, die die `strptime` Methode aus dem `datetime` Modul verwendet, um ein Datum aus einer Zeichenfolge zu parsen. 

```Python
from datetime import datetime

date_string = "12/03/2021"
date_object = datetime.strptime(date_string, "%d/%m/%Y")

print(date_object)
```

Ausgabe: 

```Python
2021-03-12 00:00:00
```

## Tiefgreifender Einblick 

Die Methode `strptime` der `datetime`-Klasse ist die übliche Methode für das Parsen von Daten. Es wurde in früheren Versionen von Python eingeführt und ist seitdem die Standardlösung für diese Aufgabe.

Eine Alternative zu `strptime` wäre die Verwendung der `dateutil.parser.parse` Funktion, die in der Lage ist, die meisten datumsähnlichen Strings automatisch zu parsen und dabei auf eine vordefinierte Formatangabe zu verzichten.

```Python
from dateutil.parser import parse

date_string = "12/03/2021"
date_object = parse(date_string, dayfirst=True)

print(date_object)
```

Hinsichtlich des Implementierungsdetails des Datums-Parsing haben Python-Entwickler versucht, den Prozess so effizient wie möglich zu gestalten. Dennoch kann der Vorgang bei großen Mengen von Daten zeitintensiv sein.

## Siehe auch

Für weitere Informationen und Dokumentationen über das Parsen von Daten aus Zeichenfolgen in Python können Sie die offizielle Python-Dokumentation besuchen: 

- [Python datetime Modul](https://docs.python.org/3/library/datetime.html)
- [Python dateutil Modul](https://dateutil.readthedocs.io/en/stable/parser.html)