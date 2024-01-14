---
title:    "C++: Läsning av en textfil"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa en textfil är en grundläggande del av programmering. Genom att läsa en textfil kan du få tillgång till och bearbeta data som är lagrad i filen. Detta kan vara användbart för att hantera stora mängder data eller för att läsa in information från externa källor.

## Hur man gör det
För att läsa en textfil i C++ behöver du först öppna filen med hjälp av "fstream" biblioteket och sedan använda kommandot "getline" för att läsa in varje rad av filen. Nedan finns ett exempel på kod tillsammans med den förväntade utmatningen.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Öppna filen för läsning
  ifstream fil("exempel.txt");

  // Skapa en variabel för att lagra varje rad
  string rad;

  // Läs in varje rad av filen med hjälp av "getline"
  while (getline(fil, rad)) {
    // Skriv ut varje rad
    cout << rad << endl;
  }

  // Stäng filen
  fil.close();

  return 0;
}
```

Om "exempel.txt" innehåller följande rader:

```
Hej!
Hur mår du?
Jag mår bra, tack.
```

Så kommer koden att producera följande utmatning:

```
Hej!
Hur mår du?
Jag mår bra, tack.
```

## Djupdykning
Förutom att använda "getline" för att läsa in varje rad, kan du också använda ">>" operatorn för att läsa in varje ord av en rad. Du kan också använda "get" funktionen för att läsa in en enskild karaktär. Detta kan vara användbart för att läsa in specifika typer av data från en textfil.

En viktig sak att tänka på när du läser en textfil är att kontrollera att filen verkligen har öppnats och att du har åtkomst till den. Om filen inte kan öppnas kan det bero på att den inte finns, att du inte har rättigheterna att öppna den, eller att sökvägen till filen är felaktig. Du bör också se till att stänga filen efter att du har läst den för att frigöra eventuella resurser som är knutna till den.

## Se också
- [C++ - Läs en textfil](https://www.programiz.com/cpp-programming/library-function/fstream/ifstream)
- [C++ - Skriv till en textfil](https://www.programiz.com/cpp-programming/library-function/fstream/ofstream)