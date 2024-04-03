---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:29.568517-07:00
description: "Hvordan: Pythons standardbibliotek tilbyr `datetime`-modulen, som inkluderer\
  \ `strptime`-metoden for dette form\xE5let. Metoden krever to argumenter:\u2026"
lastmod: '2024-03-13T22:44:40.370727-06:00'
model: gpt-4-0125-preview
summary: "Pythons standardbibliotek tilbyr `datetime`-modulen, som inkluderer `strptime`-metoden\
  \ for dette form\xE5let."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
Pythons standardbibliotek tilbyr `datetime`-modulen, som inkluderer `strptime`-metoden for dette formålet. Metoden krever to argumenter: datostrengen og et formatdirektiv som spesifiserer mønsteret til inngangsstrengen.

```python
from datetime import datetime

# Eksempelstreng
date_string = "2023-04-01 14:30:00"
# Analyser streng til datetime-objekt
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Utdata: 2023-04-01 14:30:00
```

For mer nyansert dataanalyse, spesielt når man håndterer flere formater eller lokaler, kan det tredjeparts biblioteket `dateutil` være svært nyttig. Det tilbyr en parsermodul som kan analysere datoer i nesten hvilket som helst strengformat.

```python
from dateutil import parser

# Eksempelstrenger
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Bruker dateutil sin parser
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Utdata: 2023-04-01 14:30:00
print(parsed_date2)
# Utdata: 2023-04-01 14:30:00
```

`dateutil` er dyktig til å håndtere de fleste datoformater uten eksplisitte formatstrenger, noe som gjør det til et allsidig valg for applikasjoner som håndterer forskjellige datorepresentasjoner.
