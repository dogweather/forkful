---
title:                "C++: Omvandling av datum till sträng"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift i C++ programmering, särskilt när man arbetar med användarinmatning eller lagring av datumdata. Det är viktigt att kunna arbeta med datatyper som passar dina specifika behov, och detta inkluderar att kunna konvertera dem till läsbara format som strängar.

## Hur man gör det

För att konvertera ett datum till en sträng i C++ finns det flera sätt att göra det. Ett vanligt sätt är att använda funktionen `strftime` som är en del av `<ctime>` biblioteket. Denna funktion tar in ett datumobjekt och en formatsträng som parametrar och returnerar en sträng med det formaterade datumet.

```C++
// Exempel på konvertering av datum till sträng med strftime funktionen

#include <iostream>
#include <ctime>
using namespace std;

int main() {
  time_t now = time(0); // Skapar ett datumobjekt för dagens datum och tid
  char dateBuffer[80];
  strftime(dateBuffer, 80, "%d/%m/%Y", localtime(&now)); // Konverterar datumet till en sträng i formatet dd/mm/yyyy
  cout << dateBuffer << endl; // Output: 22/03/2021
  return 0;
}
```

Det finns många olika formatsträngar som kan användas med `strftime` funktionen för att få olika format på den konverterade strängen. Det är viktigt att notera att formatet av datum och tid kan skilja sig åt beroende på vilken plattform eller operativsystem du arbetar på, så se till att testa din kod på olika enheter för att förvissa dig om att det fungerar som förväntat.

Det är också värt att nämna att `strftime` funktionen förväntar sig att ta in ett datumobjekt som är i UTC-tid, så se till att hantera tidszoner korrekt om du vill konvertera ett datum från en annan tidszon.

## Djupdykning

Som nämnts finns det många olika formatsträngar som kan användas för att formatera den konverterade datumen in på önskad vayat87, uvärsär det dock viktigt att vara medveten om att vissa tecken kan ha en annan betydelse på olika plattformar eller operativsystem. Till exempel kan `%d` som i exemplet ovan förvänta sig att visa talet i en ledande nolla på vissa operativsystem, medan det på andra system kan förväntas att visa utan en ledande nolla.

Det är också värt att nämna att det finns andra funktioner och bibliotek som kan användas för att konvertera datum till strängar i C++, såsom `std::stringstream` och `boost::format`. Valet av konverteringsmetod beror på dina specifika behov och preferenser.

## Se även

- [C++ Date and Time Manipulation](https://www.geeksforgeeks.org/c-date-and-time-manipulation-step-by-step-guide/)
- [strftime documentation](https://www.cplusplus.com/reference/ctime/strftime/)