---
title:    "Python: Att hämta aktuellt datum"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att få dagens datum är en nödvändig del av många Python-program. Det kan användas för att spåra tidsstämplar, schemalägga uppgifter eller helt enkelt visa dagens datum på en sida. Genom att lära sig hur man hämtar dagens datum i Python kan du förbättra dina programmeringsfärdigheter och öka funktionaliteten i dina projekt.

## Hur man gör det
Att få dagens datum i Python är enkelt och kräver bara några få rader kod. Det finns många inbyggda moduler i Python som gör det möjligt att hämta datum, men vi kommer att fokusera på användningen av datetime-modulen.

Först måste vi importera datetime-modulen till vårt program. Detta gör vi genom att skriva följande kod:

``` Python
import datetime
```

Efter att vi har importerat modulen kan vi använda datetime.date.today () -funktionen för att hämta dagens datum. Detta ger oss ett datetime-objekt som representerar dagens datum. För att få ut datumet i ett läsbart format kan vi använda strftime () -funktionen och ange det format vi vill ha. Här är en exempelkod som hämtar dagens datum och skriver ut det i ISO-format:

``` Python
import datetime

idag = datetime.date.today()
print(today.strftime("%Y-%m-%d"))
```

Detta kommer att producera en output som följande:

``` 
2021-06-25
```

Om du vill ha en annan format på datumet, kan du ändra strängen i strftime-funktionen. Här är några andra format som du kanske vill använda:

- %d - dag i månaden (1-31)
- %m - månad (01-12)
- %b - månadsnamn (jan-dec)
- %Y - år (2021)
- %A - dag i veckan (måndag-söndag)

Det finns också möjlighet att få ut tiden tillsammans med datumet. Detta kan göras genom att använda datetime.now () istället för datetime.date.today (). Här är en kod som hämtar både datum och tid och skriver ut det i ett specifikt format:

``` Python
import datetime

idag = datetime.now()
print(idag.strftime("Det är %H:%M på %d %B %Y."))
```

Detta ger oss en output som följande:

``` 
Det är 21:54 på 25 juni 2021.
```

## Djupdykning
Nu när vi har lärt oss hur man får dagens datum i Python, låt oss ta en djupare titt på datetime-modulen och dess funktioner. En annan användbar funktion är datetime.now (). Detta ger oss en datetime-objekt som innehåller både datum och tid just nu. Om vi vill skapa ett datetime-objekt för ett specifikt datum och tid kan vi använda datetime.datetime () -funktionen. Här är ett exempel på hur man skapar ett datetime-objekt för den 1 juni 2021 klockan 12:00:

``` Python
examendag = datetime.datetime(2021, 6, 1, 12, 00)
```

Vi kan sedan använda strftime () -funktionen för att ändra formatet på datumet och tiden enligt våra behov.

Den datetime-modulen innehåller också funktioner för att utföra beräkningar med datum och tid, till exempel att lägga till eller subtrahera dagar eller timmar från ett datetime-objekt.

## Se även
- [Python datetime-modulens dokumentation](https://docs.python.org/3/library/datetime.html)
- [W3Schools Python-datatyper](https://www.w3schools.com/python/python_datatypes.asp)
- [Real Python: Dates and Times in Python](https://realpython.com/python-datetime/)

Tack för att du läser! Hoppas du har lärt dig något nytt om hur man får dagens datum i Python. Lycka till med dina programmeringsprojekt!