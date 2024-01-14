---
title:                "C++: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en tillfällig fil är en vanlig teknik inom C++ programmering. Det är användbart när du behöver tillfälligt lagra data eller spara resultaten av en beräkning. Dessutom kan tillfälliga filer användas för att hantera tillgång till resurser som filer eller databaser.

## Så här går du tillväga
Här är en enkel kod som visar hur man skapar och använder en tillfällig fil:

```C++
#include <iostream>
#include <fstream> // inkludera den nödvändiga biblioteket för filmanipulering

int main() {
    // Skapa en objekt av typen ofstream för att skriva till en fil
    std::ofstream temp_file;
    // Använd std::tmpnam för att generera ett unikt namn för den tillfälliga filen
    char file_name[L_tmpnam];
    std::tmpnam(file_name);

    // Öppna den tillfälliga filen för skrivning
    temp_file.open(file_name);

    // Skriv några rader till den tillfälliga filen
    temp_file << "Detta är en tillfällig fil som skapats i C++!" << std::endl;
    temp_file << "Den kan användas för att spara data eller resultat av beräkningar." << std::endl;

    // Stäng filen
    temp_file.close();

    // Öppna den tillfälliga filen för läsning
    std::ifstream in_file(file_name);

    // Läs innehållet i den tillfälliga filen och skriv ut det på skärmen
    std::cout << "Innehållet i den tillfälliga filen:" << std::endl;
    std::cout << in_file.rdbuf() << std::endl;

    // Stäng filen
    in_file.close();

    // Radera den tillfälliga filen
    std::remove(file_name);

    return 0;
}
```
Följande utdata genereras av koden ovan:

```
Innehållet i den tillfälliga filen:
Detta är en tillfällig fil som skapats i C++!
Den kan användas för att spara data eller resultat av beräkningar.
```

## Fördjupning
I koden ovan användes funktionen `std::tmpnam` för att generera ett unikt namn för den tillfälliga filen. Detta är en enkel och bekväm metod, men den har en nackdel: det finns en risk för att två processer skapar filer med samma namn vid nästan exakt samma tidpunkt. För att undvika detta kan man istället använda funktionen `std::tmpfile`, som automatiskt genererar ett unikt filnamn och skapar en tom fil med det namnet.

En annan viktig aspekt att tänka på när man hanterar tillfälliga filer är att se till att de raderas när de inte längre behövs. Annars riskerar man att få en mängd oanvända filer som tar upp onödig plats på hårddisken. I koden ovan användes funktionen `std::remove` för att radera den tillfälliga filen efter att den hade använts. Det är också en god praxis att kontrollera om filen verkligen har raderats efteråt.

## Se även
- [C++ filhantering](https://www.programiz.com/cpp-programming/files-input-output)
- [C++ temporära filer](https://www.techiedelight.com/create-temporary-files-cpp/)