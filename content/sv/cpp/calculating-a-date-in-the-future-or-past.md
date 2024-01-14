---
title:    "C++: Beräkna ett datum i framtiden eller förflutna"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller i det förflutna är en vanlig uppgift inom programmering. Det kan vara användbart för att planera händelser eller för att hantera tidsbaserade data. I denna bloggpost kommer vi att titta på hur man kan göra detta i C++.

## Så här gör du

För att börja beräkna ett datum i C++, behöver vi först inkludera biblioteket `<ctime>`. Detta bibliotek innehåller funktioner för att hantera datum och tid i C++.

För att beräkna ett datum i framtiden använder vi funktionen `mktime`, som tar in ett `tm`-strukturobjekt som innehåller information om det nuvarande datumet och tiden. För att beräkna ett datum i förflutna måste vi först konvertera det nuvarande datumet till en `tm`-strukturobjekt och sedan använda funktionen `mktime` tillsammans med önskat antal sekunder som ska läggas till eller subtraheras från datumet.

Här är ett exempel på hur man kan beräkna ett datum i framtiden som är 10 dagar från det nuvarande datumet:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main(){
  // Skapar ett tm-objekt med det nuvarande datumet och tiden
  time_t now = time(0);
  tm *date = localtime(&now);

  // Lägger till 10 dagar till datumet
  date->tm_mday += 10;

  // Omvandlar datumen till sekunder och använder mktime för att beräkna det nya datumet
  now = mktime(date);

  // Skriver ut det nya datumet
  cout << "Datumet om 10 dagar kommer att vara: " << ctime(&now) << endl;

  return 0;
}
```

Kör man detta program kommer det att ge följande utmatning:

```
Datumet om 10 dagar kommer att vara: Sun Jul 19 02:32:21 2020
```

För att beräkna ett datum i förflutna kan vi göra på samma sätt, men istället för att lägga till dagar använder vi `date->tm_mday -= 10;` för att subtrahera dagar.

## Djupdykning 

Det finns flera viktiga saker att tänka på när man arbetar med datum i C++. Först och främst måste vi förstå att `mktime`-funktionen använder OS:ets tidszoninställningar för att beräkna datumet. Om tidszonen är felaktig kommer även det beräknade datumet att vara felaktigt.

En annan sak att tänka på är att `tm`-strukturobjektet använder en 24-timmars klocka, vilket betyder att timmar är representerade från 0 till 23 istället för 1 till 12. Om man behöver arbeta med en 12-timmars klocka måste man göra vissa manuella konverteringar.

## Se även

* [C++ datum och tidsfunktioner](https://www.programiz.com/cpp-programming/library-function/ctime)
* [Dokumentation för <ctime> biblioteket](https://en.cppreference.com/w/cpp/chrono/c/time)
* [Dagens datum i C++](https://www.geeksforgeeks.org/how-to-find-current-date-and-time-in-c/)