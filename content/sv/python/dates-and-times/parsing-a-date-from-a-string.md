---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:01.638253-07:00
description: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textuell\
  \ datum- och tidsinformation till ett datetime-objekt eller motsvarande strukturerat\u2026"
lastmod: 2024-02-19 22:04:56.733922
model: gpt-4-0125-preview
summary: "Att tolka ett datum fr\xE5n en str\xE4ng inneb\xE4r att konvertera textuell\
  \ datum- och tidsinformation till ett datetime-objekt eller motsvarande strukturerat\u2026"
title: "Analysera ett datum fr\xE5n en str\xE4ng"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att konvertera textuell datum- och tidsinformation till ett datetime-objekt eller motsvarande strukturerat format. Detta utförs vanligtvis för att möjliggöra datumaritmetik, jämförelser och formateringsoperationer på ett sätt som är språk- och regionsagnostiskt. Programmerare gör det för att effektivt hantera och manipulera tidsrelaterade data som extraherats från loggar, användarinmatningar eller externa källor.

## Hur:
Pythons standardbibliotek tillhandahåller `datetime`-modulen, som inkluderar metoden `strptime` för detta ändamål. Metoden kräver två argument: datumsträngen och en formatdirektiv som specificerar mönstret för inmatningssträngen.

```python
from datetime import datetime

# Exempelsträng
date_string = "2023-04-01 14:30:00"
# Tolka sträng till datetime-objekt
parsed_date = datetime.strptime(date_string, "%Y-%m-%d %H:%M:%S")

print(parsed_date)
# Utdata: 2023-04-01 14:30:00
```

För mer nyanserad datumtolkning, särskilt när man hanterar flera format eller lokaliteter, kan det tredjepartsbiblioteket `dateutil` vara extremt hjälpsamt. Det tillhandahåller en parser-modul som kan tolka datum i nästan alla strängformat.

```python
from dateutil import parser

# Exempelsträngar
date_string1 = "April 1, 2023 2:30 PM"
date_string2 = "1st April 2023 14:30"

# Använda dateutils parser
parsed_date1 = parser.parse(date_string1)
parsed_date2 = parser.parse(date_string2)

print(parsed_date1)
# Utdata: 2023-04-01 14:30:00
print(parsed_date2)
# Utdata: 2023-04-01 14:30:00
```

`dateutil` är skicklig på att hantera de flesta datumformat utan explicita formatsträngar, vilket gör det till ett mångsidigt val för applikationer som hanterar varierande datumrepresentationer.
