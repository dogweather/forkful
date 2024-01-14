---
title:                "C++: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Det kan finnas många anledningar till varför man behöver räkna ut ett datum i framtiden eller det förflutna. Det kan vara för att planera en resa, boka ett möte eller evenemang, eller helt enkelt för att hålla koll på viktiga datum.

## Så här gör du
Om du arbetar med C++ programming och behöver beräkna ett datum i framtiden eller det förflutna, finns det några enkla steg att följa.

### Steg 1: Skapa en instans av date-klassen
Först måste du skapa en instans av date-klassen i C++. Detta kan göras genom att använda konstruktorn för klassen och ange året, månaden och dagen du vill beräkna från.

```C++
date d(2021, 4, 15); // Skapar ett datumobjekt för 15 april 2021
```

### Steg 2: Använd add_days() eller add_months() funktionen
Nästa steg är att använda funktionerna add_days() eller add_months(). Dessa funktioner gör det möjligt att lägga till ett visst antal dagar eller månader till det befintliga datumet.

```C++
d.add_days(7); // Lägger till 7 dagar till datumet
d.add_months(2); // Lägger till 2 månader till datumet
```

### Steg 3: Få ut datumet i önskat format
Slutligen kan du få ut det beräknade datumet i önskat format genom att använda format()-funktionen.

```C++
cout << d.format("%Y-%m-%d"); // Skriver ut datumet i formatet ÅÅÅÅ-MM-DD (t.ex. 2021-04-22)
```

## Djupdykning
Om du vill gå på djupet och förstå mer om hur datumberäkningar fungerar inom programmering, kan det vara värt att bekanta sig med de olika datumbiblioteken som finns tillgängliga för C++. Ett av de mest populära är "Chrono library", som introducerades i C++11 och erbjuder ett brett spektrum av funktioner för datum- och tidsberäkningar.

För att förstå mer om hur man hanterar datum i C++ är det också viktigt att ha en grundläggande förståelse för datatyper och variabler i språket. Ett datum i C++ kan representeras som en variabel av typen "date", men det är också möjligt att använda andra datatyper, som "tm" eller "time_t".

## Se också
- https://stackoverflow.com/questions/2602616/c-get-date-and-time-in-a-specific-format
- https://www.learncpp.com/cpp-tutorial/89-class-code-and-header-files/
- https://www.geeksforgeeks.org/challenging-logics/
- https://www.cplusplus.com/reference/chrono/