---
title:                "C++: Beräkning av ett datum i framtiden eller förflutna"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna räkna ut en datum i framtiden eller förflutna kan vara användbart för att planera händelser eller för att hålla koll på viktiga deadlines. Det kan också vara en kul utmaning för de som gillar programmering. I denna bloggpost kommer vi att utforska hur man kan beräkna datumet i framtiden eller förflutna med hjälp av C++.

## Hur man gör det
För att kunna beräkna ett datum i framtiden eller förflutna behöver vi använda oss av några grundläggande metoder och funktioner inom C++. För att börja, låt oss först definiera de variabler som vi kommer att använda.

```C++
int currDay, currMonth, currYear;
int numDays;
```

Här kommer vi att använda `currDay`, `currMonth` och `currYear` för att lagra det aktuella datumet, och `numDays` för att ange antalet dagar som vi vill beräkna från det aktuella datumet.

För att beräkna datumet i framtiden använder vi `currDay`, `currMonth` och `currYear` för att skapa ett `tm`-objekt (time structure) som innehåller det aktuella datumet.

```C++
tm date = {0};
date.tm_year = currYear - 1900;
date.tm_mon = currMonth - 1;
date.tm_mday = currDay;
```

Vi kan sedan öka datumet med antalet dagar som vi vill beräkna genom att använda `mktime()`-funktionen. Detta kommer att konvertera vårt `tm`-objekt till sekunder och sedan lägga till antalet dagar, vilket resulterar i det nya datumet.

```C++
date.tm_mday += numDays;
mktime(&date);
```

Vi kan sedan hämta det nya datumet från `date`-objektet och skriva ut det i konsolen.

```C++
int newDay = date.tm_mday;
int newMonth = date.tm_mon + 1;
int newYear = date.tm_year + 1900;

cout << "Det nya datumet är: " << newDay << "/" << newMonth << "/" << newYear;
```

Vi kan också använda samma approach för att beräkna ett datum i förflutna, men denna gång kommer vi att minska antalet dagar istället för att öka det.

```C++
date.tm_mday -= numDays;
mktime(&date);

int newDay = date.tm_mday;
int newMonth = date.tm_mon + 1;
int newYear = date.tm_year + 1900;

cout << "Det nya datumet är: " << newDay << "/" << newMonth << "/" << newYear;
```

## Djupdykning
Det finns många sätt att beräkna ett datum i framtiden eller förflutna, men i denna bloggpost har vi fokuserat på en enkel och effektiv metod med hjälp av C++. Du kan också utforska andra metoder och funktioner inom C++ för att åstadkomma samma uppgift. Det är också viktigt att notera att C++ följer Gregorianska kalendern, där året 1900 är år 0 och inte ett skottår.

## Se även
-  [C++ referens - mktime](https://www.cplusplus.com/reference/ctime/mktime/)
- [C++ referens - Gregorianska kalendern](https://www.cplusplus.com/forum/lounge/44166/)
- [Översikt över tids- och datumfunktioner i C++](https://www.geeksforgeeks.org/date-time-header-file-time_h-2/)