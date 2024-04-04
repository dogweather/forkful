---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Wie man es macht: Python macht es einfach, Daten in Zeichenketten umzuwandeln.\
  \ Benutzen Sie die\u2026"
lastmod: '2024-04-04T02:02:38.176473-06:00'
model: gpt-4-0125-preview
summary: Python macht es einfach, Daten in Zeichenketten umzuwandeln.
title: Eine Datum in einen String konvertieren
weight: 28
---

## Wie man es macht:
Python macht es einfach, Daten in Zeichenketten umzuwandeln. Benutzen Sie die [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) Methode, die für [Datum](https://docs.python.org/3/library/datetime.html#date-objects) Objekte verfügbar ist. So geht's:

```Python
from datetime import datetime

# Das aktuelle Datum und Uhrzeit bekommen
now = datetime.now()

# In eine Zeichenkette im Format: Monat Tag, Jahr umwandeln
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Ausgabe: März 29, 2023 (oder aktuelles Datum)

# Format: JJJJ-MM-TT
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Ausgabe: 2023-03-29 (oder aktuelles Datum)
```

### Wie ich es mache

So erhalte ich ein [ISO 8601](https://www.w3.org/QA/Tips/iso-date) Formatdatum mit Zeitzone:

```python
def datestamp() -> str:
    """ 
    Das aktuelle Datum und Uhrzeit mit Zeitzone im ISO-Format.
    """
    return datetime.now().astimezone().isoformat()
```

#### Beispiel-Ausgabe:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```

## Tiefergehend
Historisch gesehen war die Umwandlung von Datum in Zeichenketten ein Standard in der Programmierung, wegen der Notwendigkeit, Daten in einem menschenlesbaren Format darzustellen.

Alternativen zu `strftime` beinhalten die Verwendung der `isoformat` Methode für das ISO 8601 Format, oder Drittanbieter-Bibliotheken wie `arrow` und `dateutil`, die flexiblere Parsing- und Formatierungsoptionen bieten.

Implementierungstechnisch steht `strftime` für "string format time" und hat seine Wurzeln in der C-Programmierung. Pythons `strftime` interpretiert Formatierungscodes wie `%Y` für das Jahr und `%m` für den Monat, was eine fast endlose Anpassungsfähigkeit ermöglicht.

## Siehe Auch
Um tiefer in Pythons Datum- und Zeitfunktionen einzutauchen:
- Die offizielle `datetime` Dokumentation von Python: https://docs.python.org/3/library/datetime.html
- Für diejenigen, die an einer umfassenden Liste von `strftime` Direktiven interessiert sind: https://strftime.org/
- Um Drittanbieter-Datum/Zeit-Bibliotheken zu erkunden:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
