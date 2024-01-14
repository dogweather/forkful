---
title:                "C: Beräkna ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför 

Att kunna räkna ut ett datum i framtiden eller det förflutna kan vara användbart i många olika sammanhang. Kanske behöver du planera en resa eller evenemang och vill veta exakt vilken dag det kommer att inträffa på. Eller så kanske du arbetar med en applikation som behöver kunna hantera olika datum. Oavsett vad det är, är det alltid en bra färdighet att ha inom programmering.

## Så här gör du 

För att kunna beräkna ett datum i framtiden eller förflutna behöver du först och främst förstå hur datum representeras i C-programmering. I C, är datum ett heltal som representerar antalet dagar från en ursprungsdatum, även kallat "epoch". Detta datum varierar beroende på vilket operativsystem du använder, men det är vanligtvis den 1 januari 1970.

För att räkna ut ett datum i framtiden eller förflutna, behöver du ta följande saker i beaktning: årtal, månad och dag. Du behöver också veta hur många dagar som finns i en viss månad och om det är ett skottår eller inte. Du kan sedan använda olika matematiska operationer för att beräkna det önskade datumet.

För att ge dig en bättre förståelse för hur detta kan se ut i kod, kommer här några exempel på hur du kan räkna ut ett datum i framtiden eller förflutna:

```
// Exempel för att räkna ut ett datum 30 dagar framåt från dagens datum

#include <stdio.h>
#include <time.h>

int main()
{
  // Skapa ett struct för dagens datum
  time_t t = time(NULL);
  struct tm *today = localtime(&t);

  // Lägg till 30 dagar
  today->tm_mday += 30;

  // Använd mktime() för att konvertera till en sekunds representation
  // Sedan konvertera tillbaka till ett struct för det nya datumet 
  t = mktime(today);
  today = localtime(&t);

  // Skriv ut det nya datumet
  printf("Datumet 30 dagar från idag är: %02d-%02d-%d\n", today->tm_mday, today->tm_mon + 1, today->tm_year + 1900);
  
  return 0;
}

```

Output:
```
Datumet 30 dagar från idag är: 27-07-2021

```

Som du kan se i det här exemplet, använder vi funktionen `mktime()` för att konvertera datumet till en sekunds representation och sedan tillbaka till ett `struct` för det nya datumet.

## Deep Dive 

För att göra din kod mer robust, kan du också överväga att hantera vissa felkällor som kan uppstå när man räknar ut ett datum. Till exempel, om det är ett skottår, så kommer februari ha 29 dagar istället för 28. Du behöver också hantera att månaden inte kan ha mer än 31 dagar. Genom att använda olika villkor och loopar kan du se till att din kod fungerar korrekt oavsett vilket datum som används.

Det kan också vara användbart att lägga till funktioner som låter användaren mata in ett datum och sedan beräkna ett nytt datum från det. Det gör att din kod blir mer mångsidig och användbar i olika scenarion.

## Se också

* [C-programmering för nybörjare](https://www.programiz.com/c-programming)
* [Dokumentation för C:s datum- och tidfunktioner](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
* [C-programmering på YouTube](https://www.youtube.com/playlist?list=PLGLfVvz_LVvRX6xK1oi0reKci6ignjdSa)