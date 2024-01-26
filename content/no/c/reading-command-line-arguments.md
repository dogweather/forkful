---
title:                "Lese kommandolinjeargumenter"
date:                  2024-01-20T17:55:23.626710-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kommandolinjeargumenter lar brukere sende data direkte til et program når de starter det. Programmerere bruker dette for å gjøre applikasjoner fleksible og for tilpasning av oppførsel uten å endre koden.

## Hvordan:
Hvert C-program får `argc` og `argv` som parametre i `main()`-funksjonen – `argc` teller argumenter, mens `argv` er en liste med argumentene.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Det er %d kommandolinjeargument(er):\n", argc);
    for (int i = 0; i < argc; i++) {
        printf("Argument %d er: %s\n", i, argv[i]);
    }
    return 0;
}
```

Kjører du programmet slik: `./program navn`, blir utskriften:

```
Det er 2 kommandolinjeargument(er):
Argument 0 er: ./program
Argument 1 er: navn
```

## Deep Dive
Kommandolinjeargumenter har vært en del av C siden starten på 70-tallet. Alternativer inkluderer å bruke miljøvariabler, interaktive prompts, eller konfigurasjonsfiler. Mange biblioteker forenkler parsing og håndtering av argumenter, slik som `getopt`. I `argv[]` er indeks 0 alltid programnavnet, og de ekte argumentene starter på indeks 1.

## See Also
- [GNU C Library: Program Argument Handling](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [cplusplus.com: Command line arguments](http://www.cplusplus.com/articles/DEN36Up4/)
- [POSIX standard: getopt](https://pubs.opengroup.org/onlinepubs/9699919799/functions/getopt.html)
