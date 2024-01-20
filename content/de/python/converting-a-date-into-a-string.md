---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Umwandeln (Konvertieren) eines Datums in eine Zeichenkette (String) ist eine gängige Aufgabe in der Python-Programmierung. Dabei wird ein Datum in einen lesbaren Text umgewandelt, den Menschen verstehen können. Dieser Vorgang ist wichtig, um Daten effizient zu speichern oder darzustellen.

## So geht's:
In Python wird das Datums-Objekt in den gewünschten Zeichenketten-Format mittels 'strftime'-Methode umgewandelt.

```python
from datetime import datetime

# aktuelles Datum und Uhrzeit bekommen
jetzt = datetime.now()

# Datum und Uhrzeit in einen String konvertieren
datum_string = jetzt.strftime("%d-%m-%Y, %H:%M:%S")
print("Das aktuelle Datum und Uhrzeit: ", datum_string)
```
_Ausgabe_

```bash
Das aktuelle Datum und Uhrzeit:  25-11-2021, 15:20:51
```

## Vertiefung
Historisch gesehen hat die Konvertierung von Daten in Zeichenketten immer eine wichtige Rolle in der Programmierung gespielt, um die Lesbarkeit und Nachvollziehbarkeit der Ausgabe zu verbessern. Alternativ kann man das 'isoformat()' oder 'ctime()' nutzen. Bei 'isoformat()' erhält man die ISO 8601 Darstellung des Datums, wobei 'ctime()' eine leicht lesbare Darstellung liefert.

```python
# ISO 8601 Format
iso_format = jetzt.isoformat()
print("ISO 8601 Format: ", iso_format)

# Leicht lesbare Darstellung
c_format = jetzt.ctime()
print("Leicht lesbare Darstellung: ", c_format)
```
_Ausgabe_

```bash
ISO 8601 Format:  2021-11-25T15:20:51
Leicht lesbare Darstellung:  Thu Nov 25 15:20:51 2021
```

'strftime' steht dabei für `string format time`, wobei man durch verschiedene Direktiven die Ausgabe anpassen kann. '%d' steht für den Tag, '%m' für den Monat, '%Y' für das Jahr, '%H' für Stunde, '%M' für Minute und '%S' für Sekunde. Es gibt noch viele weitere Direktiven, die man in der Python-Dokumentation nachlesen kann.

## Siehe auch
Weiterführende Ressourcen zur Datumskonvertierung in Python:

1. Python-Dokumentation: strftime() and strptime() Format Codes ([Link](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes))
2. Python-Dokumentation: datetime - Basic date and time types ([Link](https://docs.python.org/3/library/datetime.html))