---
title:                "Läsa in kommandoradsargument"
date:                  2024-01-20T17:55:44.247682-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument innebär att ditt C++-program kan ta emot och använda data direkt från terminalen när det startar. Det är användbart för att göra programmet flexibelt och för att kunna köra det med olika konfigurationer utan att ändra koden.

## How to:
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "Programmet har " << argc << " argument:" << std::endl;
    for (int i = 0; i < argc; ++i) {
        std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```
Kör programmet så här:
```
$ ./ditt_program Hej Världen
Programmet har 3 argument:
Argument 0: ./ditt_program
Argument 1: Hej
Argument 2: Världen
```

## Djupdykning
I historiens gryning fanns ingen standard för inmatning av argument, men C-standardbiblioteket etablerade `argc` och `argv` som huvudsättet att få tillgång till kommandoradsargument. Alternativa metoder inkluderar användning av olika parsing-bibliotek som `getopt` eller modernare läsningar med `boost::program_options`. `argc` står för "argument count" medan `argv` är "argument vector" som är en array av strängpekare som pekar på varje argument. Det är viktigt att notera att `argv[0]` är programmets namn, så räkningen av 'riktiga' argument börjar från `argv[1]`.

## Se även:
- C++ dokumentationen över <a href="http://www.cplusplus.com/reference/cstdlib/getenv/">`getenv`</a> för att läsa in miljövariabler.
- <a href="http://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html">Boost.Program_options</a> för ett mer avancerat argument parsingsbibliotek.
- <a href="https://en.cppreference.com/w/cpp/utility/program/getenv">cppreference om `std::getenv`</a> för standard-kompatibla alternativ.