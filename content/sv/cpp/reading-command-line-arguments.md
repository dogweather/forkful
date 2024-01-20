---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hantera kommandoradsargument i C++

## Vad & Varför?

Kommandoradsargument är parametrar som skickas in när programmet körs från kommandoraden. De tillhandahåller input till programmet vid nystart, vilket ger flexibilitet och möjlighet till dynamisk ändring av programmets beteende.

## Hur gör man:

C++ tillhandahåller en enkel mekanism för att hantera kommandoradsargument. `main()` funktionen kan definieras med två argument: `int argc, char *argv[]`.

```C++
#include<iostream>
int main(int argc, char *argv[]) {
    // Skriv ut alla inkommande argument
    for(int i = 0; i < argc; i++) {
        std::cout << argv[i] << std::endl;
    }
    return 0;
}
```

Om du kör detta program med `./myProgram Hello C++`, kommer utdata vara:
```
./myProgram
Hello
C++
```

## Fördjupning:

1. Historisk bakgrund: Konceptet med kommandolinjeargument härstammar från Unix:s tidiga dagar. Syftet var att låta användaren påverka programmets beteende vid start istället för interaktion vid runtime.

2. Alternativ: Library som Boost.Program_options erbjuder mer sofistikerade sätt att hantera kommandoradsargument, inklusive automatisk typkonvertering och standardvärden.

3. Implementeringsdetaljer: `char *argv[]` är en array av pointerson till tecken. Första elementet (`argv[0]`) är alltid namnet på programmet självt och ändpunkten är alltid en nullpeker.

## Se också:

1. [C++ comand line arguments](https://www.learncpp.com/cpp-tutorial/73-command-line-arguments/)
2. [Boost.Program_options documentation](https://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html)

Glöm inte, förståelse kring kommandoradsargument kan vara nyckeln till att skapa mer dynamiska och användarvänliga program.