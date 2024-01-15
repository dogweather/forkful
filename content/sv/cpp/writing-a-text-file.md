---
title:                "Att skriva en textfil"
html_title:           "C++: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

För att spara information och dela den med andra, eller för att läsa och bearbeta information. Textfiler kan också användas för att skapa en dokumentation av koden för framtida referens.

## Hur man gör

Det första steget för att skriva en textfil är att inkludera standardbiblioteket `fstream` i din C++-kod. Detta gör det möjligt att arbeta med filer. 

Först är det viktigt att öppna en fil innan du kan skriva till den. Detta kan göras genom att använda funktionen `open()` tillsammans med `ofstream`-objekt, till exempel:

```C++
#include <fstream>
using namespace std;

int main() {
    ofstream fil("minfil.txt"); // öppnar filen minfil.txt
    if (fil.is_open()) { // kontrollerar om filen är öppen
        // kod för att skriva till filen
        fil.close(); // stänger filen när vi är klara
    }
    else {
        cout << "Kunde inte öppna filen." << endl;
    }
    return 0;
}
```

När filen är öppen kan du använda olika funktioner som `<<` för att skriva till filen. Se följande exempel:

```C++
#include <fstream>
using namespace std;

int main() {
    ofstream fil("minfil.txt");
    if (fil.is_open()) {
        fil << "Detta är en textfil." << endl;
        fil << "Här kan vi skriva all slags information." << endl;
        fil << "Till exempel vår favoritfärg: blå." << endl;
        fil.close();
    }
    else {
        cout << "Kunde inte öppna filen." << endl;
    }
    return 0;
}
```

Det viktigaste att komma ihåg när du skriver till en textfil är att lägga till `endl` efter varje rad. Detta gör att texten skrivs på en ny rad i filen.

## Djupdykning

Det finns flera saker att tänka på när man skriver en textfil i C++. 

Förutom att använda `endl` kan du också använda `"\n"` för att skapa en ny rad i filen. Detta kan ibland vara användbart, till exempel om du vill skapa en tabell i filen.

Du kan också använda `getline()` för att läsa in en hel rad från filen istället för bara ett ord eller en siffra. Detta är särskilt användbart om du vill läsa in en hel mening från en textfil.

Slutligen är det viktigt att stänga filen när du är klar med den, annars kan det leda till problem i framtiden.

## Se även

Här är några andra användbara resurser för att lära sig mer om att skriva textfiler i C++:

- [C++ - File I/O](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ File Handling - GeeksforGeeks](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [C++ File Input/Output](https://www.learncpp.com/cpp-tutorial/181-input-and-output-io-streams/)