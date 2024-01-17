---
title:                "Läsning av en textfil"
html_title:           "C++: Läsning av en textfil"
simple_title:         "Läsning av en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa en textfil betyder att läsa och hämta information från en fil som innehåller text. Programmerare gör detta för att få tillgång till och använda viktig information som är lagrad i filen.

## Hur man gör:
Här är ett exempel på hur man läser in en textfil i C++:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main()
{
    // Öppna filen
    ifstream fil("exempel.txt");
    // Skapa en sträng som lagrar texten från filen
    string text;

    // Om filen kunde öppnas
    if (fil.is_open()){
        // Läs in texten från filen till variabeln "text"
        getline(fil, text);
        // Skriv ut texten
        cout << text << endl;
        // Stäng filen
        fil.close();

    } else {
        // Om filen inte kunde öppnas, skriv ut ett felmeddelande
        cout << "Kunde inte öppna filen." << endl;
    }

    return 0;
}
```

### Exempel på innehåll i "exempel.txt":
```
Hej! Det här är en textfil.
Jag innehåller användbar information.
Hoppas du kan använda mig i ditt program!
```

### Exempel på utmatning:
```
Hej! Det här är en textfil.
```

## Djupdykning:
Att läsa in textfiler är en viktig del av programmering och har funnits sedan de första språken skapades. Andra sätt att läsa in filer inkluderar binära filer, där informationen är lagrad i binär kod istället för text, och databasfiler som används för att lagra och hantera stora mängder data. I C++ finns olika funktioner och metoder för att läsa in olika filer, så det är viktigt att välja rätt metod för det specifika syftet.

## Se även:
Här är några artiklar som kan vara relevanta för att läsa textfiler i C++:

- [Filhantering i C++](https://www.programiz.com/cpp-programming/files-io)
- [Huvudprogrammeringsspråk: En kort historik](https://www.britannica.com/technology/computer-programming-language/Generations-of-languages)