---
title:                "C: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Att lära sig hur man läser in kommandoradsargument är ett viktigt steg för att bli en skicklig C-programmerare. Genom att kunna använda dessa argument kan du skriva program som är mer flexibla och anpassningsbara, vilket är ett vanligt scenario i verkliga programmeringsprojekt. Genom att läsa denna bloggpost kommer du att förstå hur kommandoradsargument fungerar och hur du kan använda dem i dina egna program.

## Så här gör du

För att läsa kommandoradsargument i C måste du först känna till funktionen `main`. Denna funktion tar emot två parametrar, `argc` och `argv`. `argc` är antalet argument som skickas till programmet, medan `argv` är en vektor av strängar som innehåller de faktiska argumenten.

För att ta reda på vad dessa argument är kan du använda en `for`-loop. Här är ett exempel på kod som skriver ut alla argument på en separat rad:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Om du kompilerar och kör detta program med kommandoradsargument, till exempel `./program arg1 arg2`, bör du få följande utmatning:

```
Argument 0: ./program
Argument 1: arg1
Argument 2: arg2
```

## Djupdykning

En viktig sak att notera är att `argv[0]` alltid kommer att vara namnet på programmet. Det betyder också att `argc` alltid kommer att vara minst 1, även om du inte skickar några argument till programmet.

En annan intressant funktion i C är `getopt`. Den ger dig möjlighet att hantera kommandoradsalternativ med flera flaggor. Du kan läsa mer om detta i dokumentationen för `getopt` eller söka efter exempel på nätet.

## Se även

- [Dokumentation för `getopt`](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [En introduktion till kommandoradsargument i C](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [En guide till `main`-funktionen i C](https://www.gnu.org/software/libc/manual/html_node/Function-Arguments.html#Function-Arguments)