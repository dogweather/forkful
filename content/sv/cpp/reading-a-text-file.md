---
title:                "C++: Läsa en textfil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför
Att läsa textfiler är en vanlig uppgift inom programmering, speciellt i C++. Det kan vara användbart för att bearbeta stora mängder data eller för att läsa in användarinmatning. Det är också en grundläggande koncept som alla blivande C++ programmerare bör behärska.

## Hur man gör det
För att läsa en textfil i C++, behöver vi först öppna filen med hjälp av `ifstream` klassen. Sedan kan vi läsa in varje rad i filen med hjälp av `getline` funktionen. Nedan följer ett exempel på en enkel kod som läser in en textfil och skriver ut dess innehåll till konsolen:

```C++
#include <iostream>
#include <fstream> // För att använda ifstream
#include <string>
using namespace std;

int main() {
    // Öppna filen för läsning
    ifstream infile;
    infile.open("textfil.txt");

    // Variabel för att lagra varje rad i filen
    string line;

    // Läs varje rad i filen och skriv ut till konsolen
    while (getline(infile, line)) {
        cout << line << endl;
    }

    // Stäng filen när vi är klara med den
    infile.close();
    return 0;
}
```

Om vi antar att vår textfil innehåller följande innehåll:

```
Hej,
Det här är en textfil.
```

Så kommer konsoloutputen att se ut så här:

```
Hej,
Det här är en textfil.
```

Vi kan också använda `infile.eof()` för att kontrollera om vi har läst hela filen innan vi stänger den. Om `eof` är `true`, betyder det att vi har nått slutet av filen och att vi kan stänga den.

## Djupdykning
När vi öppnar en fil för läsning använder vi `infile.open()` funktionen. Om filen inte kan öppnas av någon anledning, till exempel om den inte finns, kommer `infile.fail()` att returnera `true`. Detta kan vara användbart för att kontrollera om filen finns innan vi försöker öppna den. Dessutom kan vi använda `infile.good()` för att kontrollera om filen är öppen och redo att bli läst.

När vi läser en textfil i C++, är det viktigt att vi kontrollerar fel och hanterar dem på ett korrekt sätt. Om vi till exempel försöker öppna en fil som inte finns, bör vi kasta ett felmeddelande och avsluta programmet på ett ordnat sätt. Detta hjälper till att undvika oväntade kraschar och fel i vårt program.

## Se även
- [C++: Läsa och skriva filer](https://www.cplusplus.com/doc/tutorial/files/)
- [C++ ifstream Referens](https://www.cplusplus.com/reference/fstream/ifstream/)