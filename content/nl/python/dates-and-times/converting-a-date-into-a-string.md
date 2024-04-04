---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "Hoe te doen: Python maakt het eenvoudig om datums naar strings om te\
  \ zetten. Gebruik de\u2026"
lastmod: '2024-04-04T02:02:34.866133-06:00'
model: gpt-4-0125-preview
summary: Python maakt het eenvoudig om datums naar strings om te zetten.
title: Een datum omzetten naar een string
weight: 28
---

## Hoe te doen:
Python maakt het eenvoudig om datums naar strings om te zetten. Gebruik de [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior) methode beschikbaar op [datum](https://docs.python.org/3/library/datetime.html#date-objects) objecten. Hier is hoe:

```Python
from datetime import datetime

# Haal de huidige datum en tijd op
now = datetime.now()

# Zet het om naar een string in het formaat: Maand dag, Jaar
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Uitvoer: Maart 29, 2023 (of huidige datum)

# Formaat: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Uitvoer: 2023-03-29 (of huidige datum)
```


### Hoe ik het doe

Zo krijg ik een datum in [ISO 8601](https://www.w3.org/QA/Tips/iso-date) formaat met tijdzone informatie:

```python
def datestamp() -> str:
    """ 
    De huidige datum en tijd met tijdzone in ISO formaat.
    """
    return datetime.now().astimezone().isoformat()
```

#### Voorbeelduitvoer:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Diepgaand
Historisch gezien is de omzetting van datums naar strings een basis in programmering geweest vanwege de noodzaak om datums in een voor mensen leesbaar formaat te vertegenwoordigen.

Alternatieven voor `strftime` zijn onder meer het gebruik van de `isoformat` methode voor ISO 8601 formaat, of externe bibliotheken zoals `arrow` en `dateutil` die flexibelere parsing- en formatteringsopties bieden.

Wat betreft de implementatie, `strftime` staat voor "string format time" en heeft zijn wortels in C programmering. Python's `strftime` interpreteert opmaakcodes zoals `%Y` voor het jaar en `%m` voor de maand, wat bijna eindeloze aanpasbaarheid mogelijk maakt.

## Zie Ook
Om dieper in te duiken in Python's datum- en tijdfuncties:
- De officiële `datetime` documentatie van Python: https://docs.python.org/3/library/datetime.html
- Voor degenen die geïnteresseerd zijn in een uitgebreide lijst van `strftime` richtlijnen: https://strftime.org/
- Om externe datum/tijd bibliotheken te verkennen:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
