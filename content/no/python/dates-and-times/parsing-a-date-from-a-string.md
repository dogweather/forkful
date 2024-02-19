---
aliases:
- /no/python/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:29.568517-07:00
description: "\xC5 analysere en dato fra en streng inneb\xE6rer \xE5 konvertere tekstinformasjon\
  \ om dato og tid til et datetime-objekt eller et tilsvarende strukturert format.\u2026"
lastmod: 2024-02-18 23:08:53.533317
model: gpt-4-0125-preview
summary: "\xC5 analysere en dato fra en streng inneb\xE6rer \xE5 konvertere tekstinformasjon\
  \ om dato og tid til et datetime-objekt eller et tilsvarende strukturert format.\u2026"
title: Analysering av en dato fra en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å analysere en dato fra en streng innebærer å konvertere tekstinformasjon om dato og tid til et datetime-objekt eller et tilsvarende strukturert format. Dette gjøres vanligvis for å muliggjøre datoomregninger, sammenligninger og formateringsoperasjoner på en måte som er uavhengig av språk og region. Programmerere gjør dette for effektivt å håndtere og manipulere tidsmessige data hentet fra logger, brukerinndata eller eksterne kilder.

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
