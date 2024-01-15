---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Python: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum zu einer Zeichenfolge ist eine nützliche Fähigkeit für alle, die mit Datum und Zeit in ihren Python-Programmen arbeiten. Es ermöglicht die Darstellung von Datum und Zeit in einem lesbareren Format für Benutzer und vereinfacht die Verarbeitung von Daten in verschiedenen Formaten.

## Wie geht's

Die Konvertierung von Datum zu einer Zeichenfolge kann mit der `strftime()` Funktion in Python durchgeführt werden. Diese Funktion verwendet eine formatierte Zeichenfolge als Argument, um das Datum und die Zeit in das gewünschte Format umzuwandeln.

```Python
import datetime

today = datetime.datetime.today()
print(f"Heute ist der {today.strftime('%d.%m.%Y')}.")
```

Output:
Heute ist der 22.08.2021.

## Tiefer gehen

Die `strftime()` Funktion akzeptiert verschiedene Formatierungsoptionen, um das Ausgabeformat anzupassen. Zum Beispiel kann `%d` verwendet werden, um den Tag des Monats als zweistellige Zahl darzustellen, oder `%b` für eine dreistellige Abkürzung des Monatsnamens.

Es gibt auch eine `strptime()` Funktion, die das Umgekehrte von `strftime()` tut - sie konvertiert eine Zeichenfolge in ein `datetime` Objekt. Beide Funktionen sind hilfreich, um mit Datum und Zeit zu arbeiten, und es gibt viele nützliche Formatierungsoptionen, die in der Dokumentation von Python zu finden sind.

## Siehe auch

- [Python-Datums- und Zeitformate](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes)
- [Umrechnen von Zeichenfolgen in Datum und Zeit in Python](https://www.programiz.com/python-programming/datetime/strptime)