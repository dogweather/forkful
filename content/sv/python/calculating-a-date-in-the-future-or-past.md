---
title:    "Python: Beräkning av ett datum i framtiden eller det förflutna."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att räkna ut ett datum i framtiden eller det förflutna kan vara en användbar färdighet för många programmerare. Det kan hjälpa till att spåra händelser eller schemalägga uppgifter i en applikation.

## Hur man gör det

För att räkna ut ett datum i framtiden eller det förflutna använder man Python-modulen "datetime". Först måste man importera den i koden:

```Python
import datetime
```

Sedan kan vi skapa ett datumobjekt med ett angivet år, månad och dag:

```Python
datum = datetime.date(2021, 8, 29)
```

För att lägga till eller dra bort dagar från detta datum kan vi använda "timedelta" i kombination med datumobjektet:

```Python
framtid = datum + datetime.timedelta(days=7)
print(framtid) # 2021-09-05
```

På samma sätt kan vi också räkna ut ett datum i det förflutna genom att använda ett negativt värde för "days" i "timedelta".

## Djupdykning

Förutom att lägga till eller dra bort dagar, kan "timedelta" användas för att räkna ut andra tidsenheter som timmar, minuter och sekunder. Det är också möjligt att jämföra två datumobjekt för att se hur många dagar som skiljer dem åt.

En annan användbar funktion i "datetime" är möjligheten att formatera datum och tid på olika sätt. Till exempel kan man använda "strftime" för att få datumet i ett annat format än standard.

## Se även

- [Python dokumentation för datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [Enkel guide till datetime-modulen](https://realpython.com/python-datetime/)
- [Mer avancerad guide om datum- och tidsberäkningar i Python](https://www.geeksforgeeks.org/python-working-with-date-and-time-using-pytime-module/)