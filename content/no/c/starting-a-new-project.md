---
title:                "Å starte et nytt prosjekt"
date:                  2024-01-20T18:03:10.700452-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å starte et nytt prosjekt"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å starte et nytt prosjekt betyr å blaze en ny sti med kode fra scratch. Programmerere gjør dette for å løse nye problemer, utforske ideer eller bygge noe kult.

## Hvordan gjøre det:
La oss kaste oss rett inn i det. Her er hvordan du setter opp et enkelt "Hello, World!" i C.

```c
#include <stdio.h>

int main() {
    printf("Hei, Verden!\n");
    return 0;
}
```

Når du kompilerer og kjører dette, får du følgende utskrift:

```
Hei, Verden!
```

## Dypdykk
Tilbake på 1970-tallet, da C først dukket opp ved Bell Labs, handlet det om å håndtere lavnivå operasjoner effektivt. Det har vokst siden da, men enkelheten ved å starte et nytt prosjekt har forblitt ganske lik.

Du har alternativer som C++, Rust, eller Go for litt annerledes smaker av prosjektstartsprosessen. I C, en lys og rakettstart er vanlig: ingen tunge verktøy eller rammer, bare ditt favoritt tekstredigeringsprogram og C-kompilatoren.

En detalj å merke seg er at alle funksjoner i C bør erklæres før de brukes, og `main()` er startpunktet. Prosessen for å sette opp et nytt prosjekt kan omfatte å skrive en `Makefile` for å automatisere kompilering med `make` kommandoen.

## Se også
- [Learn-C.org](https://www.learn-c.org/): Interaktive veiledninger for å lære C.
- [GCC Documentation](https://gcc.gnu.org/onlinedocs/): Info om GNU Compiler Collection, en mye brukt C-kompilator.
- [The C Programming Language av Brian W. Kernighan og Dennis M. Ritchie](https://en.wikipedia.org/wiki/The_C_Programming_Language): Den klassiske boken som hjelper deg å dypdykke i C.
