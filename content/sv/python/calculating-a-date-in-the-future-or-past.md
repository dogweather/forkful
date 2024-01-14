---
title:    "Python: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Hej och välkomna till vår blogg om Python programmering! I dagens inlägg ska vi prata om hur man beräknar ett datum i framtiden eller förfluten tid. Det finns många olika anledningar till varför man skulle vilja göra detta, till exempel för att planera ett event eller för att hålla koll på viktiga datum. Låt oss ta en titt på hur man kan göra det!

## Varför

Det finns många situationer där man behöver beräkna ett datum i framtiden eller förfluten tid. Det kan vara för att hålla koll på viktiga händelser eller för att planera för framtida evenemang. Med hjälp av Python-programmering kan man enkelt skapa en funktion som gör detta åt en, vilket sparar tid och minskar risken för misstag.

## Så här gör man

För att beräkna ett datum i framtiden eller förfluten tid behöver vi använda oss av datum- och tidsmodulen i Python. För att börja måste vi importera modulen med följande kod:

```Python
import datetime
```

Sedan behöver vi definiera det datum som vi vill börja ifrån. Till exempel, om vi vill beräkna ett datum 2 veckor framåt från idag, så kan vi göra så här:

```Python
startdatum = datetime.datetime.now() #dagens datum
```

Sedan behöver vi skapa en variabel som innehåller antalet dagar eller veckor som vi vill lägga till eller dra ifrån från start datumet. Till exempel, om vi vill beräkna ett datum 2 veckor framåt från start datumet, så kan vi göra så här:

```Python
veckor = datetime.timedelta(weeks=2)
```

Till sist kan vi lägga till eller dra ifrån vårt valda antal dagar eller veckor från start datumet för att få det nya datumet. Detta kan göras med följande kod:

```Python
nytt_datum = startdatum + veckor
```

Och det är allt! Nu har vi ett nytt datum som är 2 veckor framåt från idag. Det kan se ut så här:

```Python
>>> print(nytt_datum)
2021-05-03 12:00:00 #det nya datumet
```

Detta är bara ett enkelt exempel för att visa hur man kan beräkna ett datum i framtiden. Man kan också använda sig av olika funktioner för att få datum i en specifik format eller för att göra mer komplexa beräkningar.

## Djupdykning

För de som är intresserade av att lära sig mer om beräkning av datum i Python, så finns det många olika moduler och funktioner som man kan utforska. Till exempel kan man använda sig av "calendar" modulen för att få en kalender för ett visst år eller månad. Det finns också olika funktioner för att kontrollera om ett år är ett skottår eller få antalet dagar i en viss månad.

Man kan också använda sig av "strftime" funktionen för att få datum i olika format, så som dagens namn eller tidzon. Det finns också möjlighet att göra beräkningar med datum, så som att dra ifrån en viss tidsperiod från ett datum eller jämföra två datum för att se hur lång tid det är mellan dem.

## Se även

För mer information och exempel på hur man kan använda sig av datum- och tidsmodulen i Python, så rekommenderar vi att ni läser följande resurser:

- [Python dokumentation om datum- och tidsmodulen](https://docs.python.org/3/library/datetime.html)
- [Real Python - Working with Datetime Objects](https://realpython.com/python-datetime/)
- [Programiz - Python Datetime Module](https://www.programiz.com/python-programming/datetime)

Tack för att ni läste vårt inlägg om att beräkna datum i framtiden eller förfluten tid med Python. Vi hoppas att ni har haft nytta av det och att ni fortsätter att utforska fler möjligheter