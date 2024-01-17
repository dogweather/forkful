---
title:                "Beräkning av ett datum i framtiden eller förflutnan"
html_title:           "Python: Beräkning av ett datum i framtiden eller förflutnan"
simple_title:         "Beräkning av ett datum i framtiden eller förflutnan"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att beräkna ett datum i framtiden eller förflutna är en vanlig uppgift för programmerare. Det innebär att ta ett befintligt datum och tillägga eller subtrahera ett visst antal dagar, veckor, månader eller år från det. Det här är användbart för att automatiskt generera datum för tidsbaserade uppgifter eller för att skapa en kalenderfunktion i en applikation.

# Hur man gör:
Om du vill beräkna ett datum i framtiden använder du funktionen ```datetime.timedelta()``` i Python. Till exempel, om du vill veta datumet 2 veckor från idag kan du skriva följande kod:

```
import datetime
fut_date = datetime.date.today() + datetime.timedelta(weeks=2)
print(fut_date)
```

Resultatet kommer att vara 2 veckor från dagens datum. För att beräkna ett datum i förflutna använder du istället minus-tecknet (-). Till exempel, om du vill veta datumet 6 månader tillbaka från idag kan du skriva följande kod:

```
import datetime
past_date = datetime.date.today() - datetime.timedelta(months=6)
print(past_date)
```

Resultatet kommer att vara 6 månader tillbaka från dagens datum.

# Djupdykning:
Att beräkna datum i framtiden eller förflutna är ett vanligt problem som har existerat i många år. Innan moderna programmeringsspråk som Python fanns, var det nödvändigt att göra dessa beräkningar manuellt. Idag är det dock mycket enklare med hjälp av inbyggda funktioner som ```datetime.timedelta()```.

Det finns också andra metoder för att hantera datumberäkningar, inklusive att använda bibliotek som arrow och dateutil. Dessa bibliotek erbjuder fler funktioner för att hantera tidsberäkningar och hantera tidszoner.

# Se även:
- Python officiella dokumentation om datetime: https://docs.python.org/3/library/datetime.html
- arrow bibliotek: https://arrow.readthedocs.io/en/latest/
- dateutil bibliotek: https://dateutil.readthedocs.io/en/stable/