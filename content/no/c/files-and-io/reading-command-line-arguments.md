---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:17.883044-07:00
description: "Hvordan gj\xF8re det: I C kan `main`-funksjonen utformes for \xE5 akseptere\
  \ kommandolinjeargumenter ved \xE5 bruke parameterne `int argc` og `char *argv[]`.\
  \ Her\u2026"
lastmod: '2024-03-13T22:44:41.288683-06:00'
model: gpt-4-0125-preview
summary: "I C kan `main`-funksjonen utformes for \xE5 akseptere kommandolinjeargumenter\
  \ ved \xE5 bruke parameterne `int argc` og `char *argv[]`."
title: Lese kommandolinjeargumenter
weight: 23
---

## Hvordan gjøre det:
I C kan `main`-funksjonen utformes for å akseptere kommandolinjeargumenter ved å bruke parameterne `int argc` og `char *argv[]`. Her representerer `argc` antall argumenter som er sendt, og `argv` er en array av tegnpekerstiller som lister alle argumentene. Her er et raskt eksempel for å illustrere:

```c
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Programnavn: %s\n", argv[0]);
    printf("Antall argumenter: %d\n", argc - 1);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Ved å bruke koden ovenfor, hvis programmet blir kjørt som `./programNavn -a eksempel`, vil utdata være:

```
Programnavn: ./programNavn
Antall argumenter: 2
Argument 1: -a
Argument 2: eksempel
```

Dette viser hvordan kommandolinjeargumenter kan analyseres og utnyttes i et C-program.

## Dypdykk
Konvensjonen for å sende argumenter til programmer går tilbake til de tidligste dagene av Unix. I denne tradisjonelle tilnærmingen gir `argc` og `argv` et enkelt, men kraftig grensesnitt for kommandolinjeinteraksjon, som kroppsliggjør Unix-filosofien om små, modulære verktøy som fungerer sammen. Selv om moderne språk ofte introduserer mer sofistikerte biblioteker eller rammeverk for å analysere kommandolinjeargumenter, tilbyr direktheten i Cs metode uovertruffen gjennomsiktighet og kontroll.

I nyere utviklinger har biblioteker som `getopt` i POSIX-systemer utviklet seg for å støtte mer komplekse analysedrøskler, som å håndtere lange valgnavn eller standardverdier for manglende argumenter. Men, det grunnleggende mekanismen for `argc` og `argv` forblir essensiell for å forstå hvordan programmer samhandler med sitt kjøretidsmiljø i C.

Kritikere kan argumentere for at det å håndtere `argc` og `argv` direkte kan være feilutsatt, og skyve for bruk av høyere nivå abstraksjoner. Likevel, for dem som søker å mestre finessene i C og sette pris på nyansene av dets lavnivå drift, er mestring av parsing av kommandolinjeargumenter en overgangsrite. Denne blandingen av historisk metodologi og praktisk nytte oppsummerer mye av Cs varige appell i systemprogrammering og programvareutvikling.
