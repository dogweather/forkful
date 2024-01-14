---
title:                "Python: Das aktuelle Datum abrufen"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das heutige Datum zu ermitteln kann in vielen Programmiersituationen nützlich sein, zum Beispiel für die Erstellung von Protokollen oder die Organisation von Dateien basierend auf dem Datum. Python bietet eine einfache Möglichkeit, das aktuelle Datum und die aktuelle Uhrzeit abzurufen.

## Wie

Um das heutige Datum mit Python zu erhalten, können wir das Modul "datetime" importieren und die Funktion "today()" aufrufen. Wir können auch das Format des Datums mit der Funktion "strftime()" ändern. Schauen wir uns ein Beispiel an:

```Python
import datetime

heutiges_datum = datetime.date.today()
print(heutiges_datum)

heutiges_datum_formatiert = heutiges_datum.strftime("%d-%m-%Y")
print(heutiges_datum_formatiert)
```
Die Ausgabe dieses Codes wäre:

```
2021-08-09
09-08-2021
```

Hier haben wir zuerst das heutige Datum ohne Formatierung erhalten und dann das Datum im gewünschten Format ausgegeben. Wir können auch die Uhrzeit mit der Funktion "now()" erhalten, indem wir das Modul "datetime" und die Funktion "strftime()" verwenden. Schauen wir uns dazu ein weiteres Beispiel an:

```Python
import datetime

aktuelle_uhrzeit = datetime.datetime.now()
print(aktuelle_uhrzeit)

aktuelle_uhrzeit_formatiert = aktuelle_uhrzeit.strftime("%H:%M:%S")
print(aktuelle_uhrzeit_formatiert)
```
Die Ausgabe dieses Codes wäre:

```
2021-08-09 18:20:30.123456
18:20:30
```

## Deep Dive

Wenn wir genauer in die Dokumentation von Python schauen, sehen wir, dass das Modul "datetime" eine Vielzahl von Funktionen bietet, um mit Datum und Uhrzeit zu arbeiten. Wir können zum Beispiel ein spezifisches Datum erstellen, indem wir die Funktion "date()" verwenden und ein Datum in Klammern übergeben. Schauen wir uns ein letztes Beispiel an:

```Python
import datetime

mein_geburtstag = datetime.date(1990, 5, 15)
print(mein_geburtstag)

mein_geburtstag_anderes_format = mein_geburtstag.strftime("%A, der %d. %B %Y")
print(mein_geburtstag_anderes_format)
```
Die Ausgabe dieses Codes wäre:

```
1990-05-15
Dienstag, der 15. Mai 1990
```

Wie du sehen kannst, können wir mit dem Modul "datetime" das aktuelle Datum und die Uhrzeit in verschiedenen Formaten erhalten und sogar spezifische Datumsangaben erstellen. Insgesamt bietet Python eine einfache und flexible Möglichkeit, mit Datum und Uhrzeit zu arbeiten.

## Siehe auch

- Python-Dokumentation zu "datetime": https://docs.python.org/de/3/library/datetime.html
- "How to Work with Dates in Python": https://realpython.com/python-datetime/
- "Python's strftime directives": https://strftime.org/