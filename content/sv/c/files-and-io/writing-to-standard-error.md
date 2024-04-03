---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:14.110246-07:00
description: "Hur man g\xF6r: I C anv\xE4nds str\xF6mmen `stderr` f\xF6r att skriva\
  \ ut felmeddelanden. Till skillnad fr\xE5n att skriva till standardutdata med `printf`,\
  \ kan skrivning\u2026"
lastmod: '2024-03-13T22:44:38.401034-06:00'
model: gpt-4-0125-preview
summary: "I C anv\xE4nds str\xF6mmen `stderr` f\xF6r att skriva ut felmeddelanden."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I C används strömmen `stderr` för att skriva ut felmeddelanden. Till skillnad från att skriva till standardutdata med `printf`, kan skrivning till `stderr` göras med hjälp av `fprintf` eller `fputs`. Så här kan du göra det:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Det här är ett felmeddelande.\n");

    fputs("Det här är ett annat felmeddelande.\n", stderr);
    
    return 0;
}
```

Exempelutdata (till stderr):
```
Det här är ett felmeddelande.
Det här är ett annat felmeddelande.
```

Det är viktigt att notera att även om utdata ser likadan ut som `stdout` i konsolen, blir skillnaden tydlig när omdirigering används i terminalen:

```sh
$ ./ditt_program > utdata.txt
```

Detta kommando omdirigerar endast standardutdata till `utdata.txt`, medan felmeddelanden fortfarande kommer att visas på skärmen.

## Fördjupning
Skillnaden mellan `stdout` och `stderr` i Unix-baserade system går tillbaka till de tidiga dagarna av C och Unix. Denna separation möjliggör en mer robust felsökning och loggning, eftersom det möjliggör för programmerare att omdirigera felmeddelanden oberoende av standardprogramutdata. Medan `stderr` är obuffrad som standard för att säkerställa omedelbar utdata av felmeddelanden, vilket hjälper vid felsökning av krascher och andra kritiska problem, är `stdout` vanligtvis buffrad, vilket betyder att dess utdata kan försenas tills bufferten töms (t.ex. vid programslut eller manuell tömning).

I moderna applikationer är det fortfarande relevant att skriva till `stderr`, särskilt för kommandoradsverktyg och serverapplikationer där det är avgörande att skilja mellan vanliga loggmeddelanden och fel. Dock, för mer komplex felhantering, särskilt i GUI-applikationer eller där mer sofistikerade loggningsmekanismer behövs, kan programmerare använda dedikerade loggningsbibliotek som erbjuder mer kontroll över meddelandeformatering, destinationer (t.ex. filer, nätverk) och allvarlighetsnivåer (info, varning, fel osv.).

Även om `stderr` erbjuder en grundläggande mekanism för felrapportering i C, innebär utvecklingen av programmeringspraxis och tillgången till avancerade loggningsramverk att det ofta bara är utgångspunkten för moderna strategier för felhantering.
