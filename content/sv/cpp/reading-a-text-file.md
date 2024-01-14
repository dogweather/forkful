---
title:                "C++: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att läsa en textfil är en vanlig uppgift för programmerare, oavsett om de arbetar med webbutveckling, databashantering eller mjukvaruutveckling. Att kunna läsa en textfil är en grundläggande färdighet som kan öppna upp möjligheter för att automatisera uppgifter och bearbeta stora mängder data. I denna bloggpost kommer vi att utforska hur man kan läsa en textfil i C++ och vilken nytta det kan ha.

## Så här

För att läsa en textfil i C++ behöver vi först öppna filen med en instans av klassen `ifstream`. Detta görs genom att skicka filnamnet som en parameter till konstruktorn. Sedan kan vi använda funktionen `getline()` för att läsa en rad i taget från filen. Nedan följer ett enkelt exempel på hur man kan läsa en textfil och skriva ut varje rad till konsolen:

```C++
#include <iostream>
#include <fstream>
#include <string>

using namespace std;

int main() {
    // Skapar en instans av ifstream och öppnar filen "textfil.txt"
    ifstream inFile("textfil.txt");

    // Om filen är öppen fortsätter vi att läsa
    if (inFile.is_open()) {
        string line;
        // Läser en rad i taget och skriver ut den till konsolen
        while (getline(inFile, line)) {
            cout << line << endl;
        }
        // Stänger filen när vi är klara
        inFile.close();
    } else {
        // Annars skriver vi ut ett felmeddelande
        cout << "Kunde inte öppna filen" << endl;
    }
    return 0;
}
```

Om vi till exempel har en textfil med namnet "textfil.txt" som innehåller följande rader:

```
Detta är en textfil.
Här finns flera rader.
Vi kan läsa dem en i taget.
```

Så kommer koden ovan att skriva ut följande till konsolen:

```
Detta är en textfil.
Här finns flera rader.
Vi kan läsa dem en i taget.
```

## Djupdykning

Förutom att bara läsa en rad i taget, finns det flera andra sätt att läsa en textfil i C++. Om vi till exempel inte vill läsa hela raden, utan bara en viss del av den, kan vi använda funktionen `get()` för att läsa en enskild karaktär från filen. Vi kan också använda funktionen `seekg()` för att hoppa till en viss position i filen och sedan fortsätta från där.

Det finns också möjlighet att läsa en textfil på en rad i taget genom att använda funktionen `get()` tills dess att vi stöter på tecknet `'\n'` som indikerar en radbrytning.

Det finns många fler funktioner och metoder som kan användas för att läsa en textfil i C++, och det är en bra idé att läsa på om dessa om man planerar att jobba med textfiler regelbundet.

## Se även

- [C++ ifstream referens](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ getline() referens](https://www.cplusplus.com/reference/string/string/getline/)
- [C++ get() referens](https://www.cplusplus.com/reference/istream/istream/get/)
- [C++ seekg() referens](https://www.cplusplus.com/reference/istream/istream/seekg/)