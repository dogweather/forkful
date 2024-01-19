---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "C: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Beräkning av ett framtida eller förflutet datum är processen för att hitta ett specifikt datum tekniskt sett framåt eller bakåt i tid från ett givet datum. Programmerare gör det ofta för att hantera och manipulera tidsbaserade data.

## Så här gör du:
Nedan följer ett exempel på C-kod som visar hur man beräknar ett datum i framtiden.

```C
#include <stdio.h>
#include <time.h>

int main ()
{
   time_t nu; 
   struct tm *datum;
   char buffer [80];

   time (&nu);

   datum = localtime (&nu);

   datum->tm_mday += 7;   // lägger till 7 dagar till dagens datum.

   strftime (buffer, 80, "%d-%m-%Y", datum);

   puts (buffer);

   return 0;
}
```
Provutgång: Om dagens datum är "09-05-2022", kommer utmatningen att vara "16-05-2022".

## Deep Dive:
Historiesk kontext: Tidsberäkningar i programmering, speciellt i C, har alltid varit en utmaning på grund av komplikationerna vid hantering av sekunder, minuter, timmar, dagar, veckor och så vidare. Det finns också problem som skottår och tidszonshanteringen att tänka på.

Alternativ: Det finns andra sätt att hantera datum, t.ex. användning av tredjepartsbibliotek som "date.h" eller "Boost Date_Time" som erbjuder mer mångsidiga funktioner för datum- och tidsberäkning.

Implementeringsdetaljer: C's time.h bibliotek erbjuder begränsad funktionalitet, men det är tillräckligt för grundläggande datummanipulering. Funktionen `localtime` använder `time_t` objektet för att fylla en `struct tm` med data uppdelade i tidselement (dag, månad, år, och så vidare). Notera dock att detta kan generera fel ifall antalet dagar överskrider månadens dagantal.

## Se også:
1. [Arbeta med datum i C](https://stackoverflow.com/questions/1442116/how-to-get-the-date-of-7-days-ago-in-c)
2. [C Date Time-funktioner](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
3. [Boost Date_Time Library](https://www.boost.org/doc/libs/1_73_0/doc/html/date_time.html)