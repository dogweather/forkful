---
title:    "C++: Omvandla ett datum till en sträng"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

När du arbetar med C++-programmering, kan du ibland behöva konvertera en datumvariabel till en sträng för att enklare kunna hantera den. Detta kan vara användbart om du till exempel vill skriva ut datumet i en viss format eller spara det i en textfil. I denna bloggpost kommer jag att gå igenom hur du kan konvertera ett datum till en sträng och ge dig en djupare förståelse för processen.

## Så här gör du

För att konvertera ett datum till en sträng, behöver du först en variabel som innehåller datumet. Detta kan vara i form av en instans av klassen `std::tm` från standardbiblioteket `chrono`. Denna klass innehåller datuminformation som år, månad och dag.

För att skapa en instans av `std::tm` med ett specifikt datum kan du använda funktionen `std::get_time`. Denna funktion tar in en sträng som representerar datumet och ett format som beskriver hur strängen ska tolkas. Detta format kan vara något av följande:

- "%d/%m/%Y" - för datum på formatet dag/månad/år
- "%m/%d/%Y" - för datum på formatet månad/dag/år
- "%Y-%m-%d" - för datum på formatet år-månad-dag

Efter att ha skapat instansen av `std::tm`, kan du använda funktionen `std::strftime` för att konvertera datumet till en sträng. Denna funktion tar in ett format för hur strängen ska se ut och returnerar en sträng som representerar datumet.

Låt oss titta på ett konkret exempel på hur detta kan se ut i kod:

```C++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

int main()
{
    std::tm date;
    std::istringstream ss("12/25/2021");
    ss >> std::get_time(&date, "%m/%d/%Y");

    char buffer[80];
    std::strftime(buffer, sizeof(buffer), "Det är %A, den %d:e %B %Y.", &date);

    std::cout << buffer << std::endl;
}
```

I detta exempel skapar vi först en instans av `std::tm` och använder sedan `std::get_time` för att ladda in ett datum från en sträng (i detta fall används formatet för månad/dag/år). Efter det använder vi `std::strftime` för att konvertera datumet till en sträng enligt det önskade formatet.

När vi kör programmet, skulle output vara:

```
Det är lördag, den 25:e december 2021.
```

## Djupdykning

När du använder `std::strftime` för att konvertera ett datum till en sträng, är det viktigt att känna till att valideringen av det angivna formatet är på programmerarens ansvar. Detta innebär att om du till exempel använder ett ogiltigt format (t.ex. "%j") kommer programmet att fortsätta köra, men resultatet kommer att vara inkorrekt.

En annan viktig detalj att notera är att `std::strftime` returnerar en nollterminerad sträng, vilket innebär att den avslutas med ett nulltecken för att markera slutet. Därför är det viktigt att din strängvariabel är tillräckligt stor för att rymma detta extra nolltecken.

## Se även

- [std::get_time](https://en.cppreference.com/w/cpp/io/manip/get_time)
- [std::strftime](https://en.cppreference.com/w/cpp/chrono/c/strftime)