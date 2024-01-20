---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Kommandoradsargument läses in för att överföra information till ett program vid körning. Detta skapar mer dynamiska program som kan hantera olika uppgifter.

## Hur gör man:

Följande kod demonstrerar hur man läser in kommandoradsargument i C:

```C
#include <stdio.h>

int main(int argc, char **argv) {
    for(int i = 0; i < argc; i++) {
        printf("Argument %d : %s\n", i, argv[i]);
    }
    return 0;
}
```

Om du kör programmet med `./myProgram Hello World`,
Visar outputet:

```
Argument 0 : ./myProgram
Argument 1 : Hello
Argument 2 : World
```

## Djupdykning:

1. Historisk bakgrund: Konceptet med kommandoradsargument går tillbaka till de tidiga dagarna av UNIX, där program utformades för att vara små och effektiva verktyg. Argumenten var vanligtvis filnamn eller datasträngar som skulle bearbetas.
2. Alternativ: Alternativen till kommandoradsargument inkluderar interaktiva användarinput, filinput och internet-kommunikation. Dock är kommandoradsargument det enklaste och mest direkt sättet att ange input till ett program vid körning.
3. Implementeringsdetaljer: I C är `argv` en dubbelpekare till en array av strängar. Antalet strängar (argument) överförs som `argc`. Första argumentet (`argv[0]`) är alltid programmets körbara namn.

## Se också:

1. [C Library - <stdio.h>](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm): Djupdykning till C's stdio.h library.
2. [Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/): En annan bra artikel om kommandoradsargument.
3. [C Programming for Beginners](https://www.udemy.com/course/c-programming-for-beginners-/): En Udemy-kurs för nybörjare i C programmering.