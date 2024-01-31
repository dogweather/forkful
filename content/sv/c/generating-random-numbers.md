---
title:                "Generera slumptal"
date:                  2024-01-27T20:33:11.186014-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga tal i C innebär att skapa sekvenser av tal som saknar något urskiljbart mönster, vilket efterliknar konceptet av slumpmässighet. Programmerare använder slumpmässiga tal för en mängd syften, inklusive simulering av data, kryptografiska tillämpningar och spelutveckling, vilket gör det till en viktig aspekt av programmering.

## Hur man gör:

För att generera slumpmässiga tal i C använder du vanligtvis `rand()`-funktionen som finns i `stdlib.h`. Dock är det avgörande att sådda (seed) slumptalsgeneratorn för att säkerställa variabilitet i de genererade talen över olika programkörningar. Funktionen `srand()`, såddad med ett värde, ofta den aktuella tiden, underlättar detta.

Här är ett enkelt exempel på att generera ett slumpmässigt tal mellan 0 och 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Sådda slumptalsgeneratorn
    srand((unsigned) time(NULL));

    // Generera ett slumpmässigt tal mellan 0 och 99
    int slumpTal = rand() % 100;

    // Skriv ut det slumpmässiga talet
    printf("Slumpmässigt Tal: %d\n", slumpTal);

    return 0;
}
```

Exempel på utdata:

```
Slumpmässigt Tal: 42
```

Det är viktigt att notera att varje körning av detta program kommer att producera ett nytt slumpmässigt tal, tack vare såddningen med den aktuella tiden.

## Fördjupning

Det traditionella sättet att generera slumpmässiga tal i C, med användning av `rand()` och `srand()`, är inte verkligt slumpmässigt. Det är pseudoslumpmässigt. Detta är bra för många tillämpningar, men det faller kort i situationer som kräver höga grader av slumpmässighet, såsom i seriösa kryptografiska användningar. Sekvensen som genereras av `rand()` bestäms helt av det värde som ges till `srand()`. Således, om såddvärdet är känt, kan sekvensen förutsägas, vilket minskar slumpmässigheten.

Historiskt sett har `rand()`-funktionen kritiserats för sin låga kvalitet på slumpmässigheten och begränsade omfång. Moderna alternativ inkluderar att använda enhetsspecifika API:er eller externa bibliotek som bättre närmar sig verklig slumpmässighet eller, i UNIX-lika system, att läsa från `/dev/random` eller `/dev/urandom` för kryptografiska ändamål.

Till exempel, användning av `/dev/urandom` i C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int slumpTal;

    // Öppna /dev/urandom för läsning
    fp = fopen("/dev/urandom", "r");

    // Läs ett slumpmässigt tal
    fread(&slumpTal, sizeof(slumpTal), 1, fp);

    // Skriv ut det slumpmässiga talet
    printf("Slumpmässigt Tal: %u\n", slumpTal);

    // Stäng filen
    fclose(fp);

    return 0;
}
```

Denna metod läser direkt från systemets entropipool och erbjuder en högre kvalitet på slumpmässigheten lämplig för mer känsliga tillämpningar. Dock kan detta tillvägagångssätt ha portabilitetsproblem över olika plattformar, vilket gör det mindre universellt än att använda `rand()`.

Oavsett metod är förståelsen för slumpmässighetens natur och dess implementering i C avgörande för att utveckla effektiva, säkra och engagerande applikationer.
