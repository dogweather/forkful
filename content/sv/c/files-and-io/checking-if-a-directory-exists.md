---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:05.368505-07:00
description: "Hur man g\xF6r: I C kan existensen av en katalog kontrolleras med hj\xE4\
  lp av `stat`-funktionen, som h\xE4mtar information om filen eller katalogen p\xE5\
  \ en angiven\u2026"
lastmod: '2024-03-13T22:44:38.398949-06:00'
model: gpt-4-0125-preview
summary: "I C kan existensen av en katalog kontrolleras med hj\xE4lp av `stat`-funktionen,\
  \ som h\xE4mtar information om filen eller katalogen p\xE5 en angiven s\xF6kv\xE4\
  g."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur man gör:
I C kan existensen av en katalog kontrolleras med hjälp av `stat`-funktionen, som hämtar information om filen eller katalogen på en angiven sökväg. Makrot `S_ISDIR` från `sys/stat.h` används sedan för att utvärdera om den hämtade informationen motsvarar en katalog.

Så här kan du använda `stat` och `S_ISDIR` för att kontrollera om en katalog existerar:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Sökvägen till katalogen som ska kontrolleras
    char *dirPath = "/path/to/directory";

    // Få statusen för sökvägen
    int result = stat(dirPath, &stats);

    // Kontrollera om katalogen existerar
    if (result == 0 && S_ISDIR(stats.st_mode)) {
        printf("Katalogen existerar.\n");
    } else {
        printf("Katalogen finns inte.\n");
    }

    return 0;
}
```

Exempelutskrift:
```
Katalogen existerar.
```

Eller, om katalogen inte finns:
```
Katalogen finns inte.
```

## Fördjupning:
Strukturen och funktionen `stat` har varit en del av programmeringsspråket C i årtionden, härstammande från Unix. De tillhandahåller ett standardiserat sätt att hämta information från filsystemet, som, trots att det är relativt lågnivå, är brett använt på grund av dess enkelhet och direkta tillgång till filsystemets metadata.

Historiskt sett har kontroll av existens och egenskaper hos filer och kataloger med `stat` och dess derivat (som `fstat` och `lstat`) varit en vanlig tillvägagångssätt. Dock interagerar dessa funktioner direkt med operativsystemets kärna, vilket kan introducera överhuvudtagande och potentiella fel om det inte hanteras korrekt.

För nya projekt eller när man arbetar i högnivåscenarier kanske programmerare väljer mer abstraherade fälthanteringsmekanismer som tillhandahålls av moderna ramverk eller bibliotek som hanterar fel mer nådigt och tillhandahåller ett enklare API. Ändå, att förstå och kunna använda `stat` förblir en värdefull kompetens för scenarier som kräver direkt manipulation av filsystemet, såsom systemprogrammering eller när man arbetar i begränsade miljöer där beroenden på stora bibliotek är opraktiskt.
