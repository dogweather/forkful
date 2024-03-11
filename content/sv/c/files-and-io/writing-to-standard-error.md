---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:14.110246-07:00
description: "Att skriva till standardfel i C inneb\xE4r att styra felmeddelanden\
  \ och diagnostisk information till en separat str\xF6m fr\xE5n programmets huvudutdata.\u2026"
lastmod: '2024-03-11T00:14:11.805552-06:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel i C inneb\xE4r att styra felmeddelanden och\
  \ diagnostisk information till en separat str\xF6m fr\xE5n programmets huvudutdata.\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva till standardfel i C innebär att styra felmeddelanden och diagnostisk information till en separat ström från programmets huvudutdata. Programmerare gör detta för att skilja felmeddelanden från standardutdata, vilket gör båda lättare att läsa och behandla separat, särskilt vid felsökning eller loggning av programkörningar.

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
