---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Arduino: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift för programmerare. Det innebär att man tar ett givet datum och adderar eller subtraherar ett specificerat antal dagar, veckor, månader eller år för att få ett nytt datum.

Det kan användas för att skapa tidsintervall, schemaläggning eller för att hantera tidsbaserade händelser. Programmerare använder det för att göra sina program mer dynamiska och anpassningsbara efter olika användningsfall.

## Så här fungerar det:
För att beräkna ett datum i framtiden eller det förflutna kan du använda funktionen ```add/subtract()``` tillsammans med variabeln ```millis()```. Här är ett exempel på hur man kan göra det för att få datumet 30 dagar efter det nuvarande datumet:

```
Arduino add/subtract(millis(), DAY, 30);
```

Output:
```Thu Mar 24 2022 22:47:05 GMT+0100 (Central European Standard Time)```

På samma sätt kan du ange antalet veckor, månader eller år istället för dagar för att få ett datum i framtiden eller det förflutna.

## Djupdykning:
Att kunna beräkna datum i framtiden eller det förflutna är en viktig del av programmering, speciellt inom områden som IoT och embedded systems. Det är också en del av mer avancerade funktioner i språk som C++.

Alternativ till att använda ```add/subtract()``` funktionen inkluderar att skapa egna funktioner eller metodik för att hantera datumberäkningar. Detta kräver dock mer kod och kan vara mer komplicerat.

För att få tillgång till alla funktioner som behövs för att beräkna datum i framtiden eller det förflutna, behöver du inkludera biblioteket DateTime.h.

## Se även:
- [Millisecond Timer Library](https://github.com/PaulStoffregen/MillisTimer)
- [Date and Time Functions Reference](https://www.arduino.cc/reference/en/libraries/datetime/)