---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "C++: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Du kanske undrar varför man skulle vilja beräkna ett datum i framtiden eller det förflutna. Kanske håller du på att skapa en kalenderapplikation eller behöver planera en viktig händelse långt i förväg. Oavsett anledning kan det vara användbart att kunna beräkna datum på ett enkelt sätt med hjälp av C++.

## Så här gör du

Att beräkna ett datum i framtiden eller det förflutna kan göras på olika sätt, men vi kommer att fokusera på en enkel metod som använder sig av några inbyggda funktioner i C++. Här är ett exempel på hur du kan göra det:

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
  // Beräkna ett datum 20 dagar framåt
  time_t now = time(0); // Hämta nuvarande tid
  tm *ltm = localtime(&now); // Skapa ett tm-objekt baserat på nuvarande tid

  // Lägg till 20 dagar på dagens datum
  ltm->tm_mday += 20; // tm_mday representerar dag i månaden
  mktime(ltm); // Konvertera tillbaka till en time_t-variabel

  // Skriv ut det nya datumet
  cout << "Nytt datum: " << ltm->tm_mon + 1 << "/" << ltm->tm_mday << "/" << ltm->tm_year + 1900 << endl;
  
  return 0;
}
```

I det här exemplet använder vi funktionerna `time()` och `localtime()` för att hämta nuvarande tid och skapa ett tm-objekt som representerar detta datum. Sedan lägger vi till det antal dagar vi vill beräkna på `tm_mday` och konverterar sedan tillbaka till en time_t-variabel med hjälp av funktionen `mktime()`. Slutligen skriver vi ut det nya datumet i formatet månad/dag/år.

Detta är bara ett grundläggande exempel, det finns många andra sätt att beräkna datum i C++. Det är också viktigt att notera att detta exempel bara fungerar för att beräkna datum i framtiden. För att beräkna datum i det förflutna kan du använda funktionen `difftime()` istället.

## Deep Dive

Om du vill fördjupa dig mer i beräkning av datum i C++ finns det en mängd olika bibliotek och externa funktioner som kan hjälpa till. Till exempel finns det bibliotek som är speciellt utformade för att hantera datum och tid, som [Boost Date Time library](https://www.boost.org/doc/libs/1_66_0/doc/html/date_time.html).

Ett annat sätt att lära sig mer om datumberäkning i C++ är att titta på koden för redan existerande kalenderapplikationer eller tidshanteringssystem. Genom att undersöka hur dessa program hanterar datum och tid kan du få en djupare förståelse för olika metoder och tekniker som används.

## Se även

- [Boost Date Time library](https://www.boost.org/doc/libs/1_66_0/doc/html/date_time.html)
- [Dokumentation för C++ Date and Time](https://www.cplusplus.com/reference/ctime/)
- [GitHub-repositorium för C++ Calendar](https://github.com/elviswolcott/cpp_calendar)