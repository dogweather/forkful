---
title:                "C++: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför läsa kommandoargument?

Att kunna läsa kommandoargument är en viktig del av att kunna skriva effektiva program. Det gör det möjligt för programmet att ta emot input från användaren och göra anpassade handlingar baserade på den inputen.

## Så här gör du

För att läsa kommandoargument i C++, behöver du först importera biblioteket "iostream" och deklarera "int main" funktionen. Sedan kan du använda "int argc" och "char* argv[]" parametrar för att ta emot argumenten från kommandoraden.

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    // Här kan du utföra dina handlingar baserat på argumenten
    // Till exempel, för att skriva ut det första argumentet:
    std::cout << argv[0] << std::endl;

    return 0;
}
```

Om du kör detta program och anger "hello" som argument från kommandoraden, så kommer programmet att skriva ut "hello" som output.

## Djupdykning

En intressant egenskap hos kommandoargument är möjligheten att läsa in argumenten som nummer istället för strängar. Detta görs genom att konvertera "argv[]" till "int" eller "double". Detta öppnar upp för olika användningsområden, som till exempel att göra matematiska beräkningar baserat på input från användaren.

## Se även

- [C++ - Kommandoargument ](https://www.w3schools.com/cpp/cpp_command_line.asp)
- [Kommandoargument i C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Argument med kommandoraden i C++](https://www.cplusplus.com/articles/DEN36Up4/)