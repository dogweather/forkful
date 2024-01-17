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

## Vad & Varför? 
Att skriva en textfil innebär att spara information i ett dokument, utan någon speciell formatering eller design. Programerare använder textfiler för att spara och återanvända data från sina program, som kan inkludera text, siffror eller annan data.

## Hur man gör:
Här kommer ett exempel på hur man skriver till en textfil i C++. Vi använder oss av ofstream-funktionen som tillåter oss att skriva och skapa nya textfiler.

```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    string text = "Hej, här är ett exempel på en textfil.";
    // Skapar och öppnar en textfil
    ofstream file("textfil.txt");
    // Skriv till textfilen
    file << text;
    // Stäng filen
    file.close();
    return 0;
}

```

När du öppnar textfil.txt kommer det att stå "Hej, här är ett exempel på en textfil.". Det är så enkelt det är att skriva en textfil!

## Deep Dive
Att spara data i en textfil är en vanlig del av programmering sedan långt tillbaka. Textfiler används ofta för att skapa en lättläst och portabel version av data som kan användas mellan olika program och system. Det finns också alternativ för att skriva till binära filer, som sparar datan i ett binärt format, men det är ofta mer komplicerat och svårare att läsa av för människor. 

När du öppnar en textfil kan du välja att öppna den som en vanlig textfil eller som en binär fil beroende på vilket syfte du har. Om du vill läsa och ändra i textfilen är det lättare att göra det som en textfil, medan en binär fil är bättre lämpad för information som inte behöver redigeras.

## Se även:
Här är några länkar som kan vara intressanta för dig som vill lära dig mer om att skriva textfiler i C++:
- [C++ Text File I/O (w3schools)](https://www.w3schools.com/cpp/cpp_files.asp)
- [C++ File Handling Tutorial (GeeksforGeeks)](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Writing files in C++ (cplusplus.com)](http://www.cplusplus.com/doc/tutorial/files/)