---
title:                "Omvandla ett datum till en sträng"
html_title:           "C++: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är en viktig och vanligt förekommande operation i C ++ programmering. Detta gör det möjligt att visa datumet i ett läsbart format för användare eller att spara datumet i en databas.

## Hur man gör

För att konvertera ett datum till en sträng i C ++, används strömmar och biblioteket "ctime". Här är ett exempel som visar hur man konverterar dagens datum till en sträng:

```C++
#include <iostream>
#include <ctime> // Bibloteket för tid och datum funktioner
using namespace std;

int main() {
  time_t now = time(0); // Skapar ett objekt som representerar nuvarande tidpunkten
  char* str = ctime(&now); // Används för att konvertera "now" till en sträng
  cout << "Idag är datumet: " << str << endl; // Skriver ut datumet
  return 0;
}
```

Detta kodexempel kommer att producera output som ser ut så här:

```
Idag är datumet: Thu Nov 5 12:14:10 2020
```

## Djupdykning

För att ändra formatet på datumet som visas i strängen, kan man använda funktionen "strftime". Detta gör det möjligt att anpassa datumet med hjälp av specifika formatsträngar. I följande exempel är formatsträngen "%d-%m-%Y" för dag/månad/år.

```C++
#include <iostream>
#include <ctime> // Bibloteket för tid och datum funktioner
using namespace std;

int main() {
  time_t now = time(0); // Skapar ett objekt som representerar nuvarande tidpunkten
  char str[80]; // Skapar en ny sträng med 80 tecken
  strftime(str, 80, "%d-%m-%Y", localtime(&now)); // Används för att konvertera och formatera datumet
  cout << "Idag är datumet: " << str << endl; // Skriver ut datumet
  return 0;
}
```

Output för detta exempel kommer att se ut så här:

```
Idag är datumet: 05-11-2020
```

## Se även

- C++ Referens - <http://www.cplusplus.com/reference/ctime/> 
- C++ Datum och tid - <http://www.cplusplus.com/reference/ctime/>