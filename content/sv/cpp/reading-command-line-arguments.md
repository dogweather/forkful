---
title:    "C++: Läsning av kommandolinjeargument"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa in argument från kommandoraden är en mycket användbar funktion inom C++ programmering. Det är ett sätt att ge flexibilitet till dina program genom att få användaren att ge in data när de kör programmet istället för att ha hårdkodade värden direkt inuti koden. Läsning av kommandoradsargument är också vanligt inom kommandoradsbaserade program, så det är en viktig färdighet att behärska.

## Så här gör du

För att läsa in kommandoradsargument i C++, behöver du inkludera biblioteket `<iostream>` och använda funktionen `int main(int argc, char* argv[])`. För att få åtkomst till de faktiska argumenten, använder du en loop som går igenom varje element i `argv` (argv[0] är alltid programnamnet) och skriver ut dem eller använder dem i din kod.

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    for (int i = 0; i < argc; i++) {
        std::cout << argv[i] << std::endl; // Skriver ut argumentet
    }
    return 0;
}
```

Om du kör programmet från kommandoraden med argumenten "hello" och "world" kommer output att vara:

```
./program hello world
```

Hello
World

## Djupdykning

`argc` representerar antalet argument som skickas in till programmet, medan `argv` är en array av strängar som håller de faktiska argumenten. Det finns också andra sätt att hantera kommandoradsargument, som att använda `std::string` eller `getopt()` funktionen. Det är viktigt att vara medveten om att argument är olika typer av data, så det kan krävas konverteringar när du använder dem i din kod.

## Se även

- [C++ Kommandoradsargument reference](https://en.cppreference.com/w/cpp/language/main_function)
- [Using getopt() to Parse Command-Line Arguments in C++](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [C++ String Class](https://www.cplusplus.com/reference/string/string/)