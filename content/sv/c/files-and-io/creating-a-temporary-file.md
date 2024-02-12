---
title:                "Skapa en tillfällig fil"
aliases:
- /sv/c/creating-a-temporary-file/
date:                  2024-02-03T17:55:51.091691-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skapa en tillfällig fil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skapa en temporär fil i C innebär att generera en fil som är avsedd att användas under en kort tid, vanligtvis som skräputrymme för databehandling eller lagring. Programmerare gör det för att hantera tillfälliga data utan att påverka programmets permanenta lagring eller för att säkerställa att känsliga data raderas efter användning.

## Hur:
Att skapa en temporär fil i programmeringsspråket C kan utnyttja funktioner såsom `tmpfile()` och `mkstemp()`.

**Använda `tmpfile()`**: Denna funktion skapar en unik temporär fil som automatiskt tas bort när programmet avslutas eller filen stängs.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp == NULL) {
        perror("Misslyckades med att skapa temporär fil");
        return 1;
    }

    // Skriv data till den temporära filen
    fputs("Detta är ett test.\n", temp);

    // Spola tillbaka och läs vad vi skrev
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Tas automatiskt bort vid stängning eller programavslut
    fclose(temp);

    return 0;
}
```
**Exempelutdata:**
```
Detta är ett test.
```

**Använda `mkstemp()`**: Ger mer kontroll över den temporära filens plats och dess behörigheter. Det kräver en mallsträng som slutar med `XXXXXX` som den sedan ersätter med en unik sekvens för att förhindra namnkollisioner.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char mall[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(mall);

    if (fd == -1) {
        perror("Misslyckades med att skapa temporär fil");
        return 1;
    }
    
    printf("Temporär fil skapad: %s\n", mall);

    // Temporära filer skapade med mkstemp() bör tas bort manuellt
    unlink(mall);

    close(fd);
    return 0;
}
```
**Exempelutdata:**
```
Temporär fil skapad: /tmp/mytemp-abc123
```

## Djupdykning
Konceptet med temporära filer är inte unikt för C utan är en vanlig funktionalitet i många programmeringsmiljöer på grund av dess nytta i hanteringen av flyktiga data. `tmpfile()`-funktionen, standardiserad i ISO C-standarden, skapar en fil med ett unikt namn i en standardkatalog, men dess existens är flyktig, vilket gör den idealisk för säkra eller tillfälliga operationer.

En anmärkningsvärd begränsning av `tmpfile()` är dess beroende av den förvalda tillfälliga katalogen, vilket kanske inte passar alla applikationer, särskilt när det gäller behörigheter eller säkerhet. I kontrast till detta låter `mkstemp()` specificera katalogen och säkerställer säker filskapande med garanterat unika filnamn genom att modifiera den angivna mallsträngen, vilket erbjuder en mer mångsidig lösning på bekostnad av manuell filhantering.

Dock kan skapandet av temporära filer introducera säkerhetsrisker, såsom loppvillkor, om de inte hanteras korrekt. Till exempel, `tmpfile()` och `mkstemp()` adresserar olika aspekter av säker temporär filskapande (automatisk borttagning och säker namngenerering, respektive), men ingen är en universallösning. Utvecklare måste överväga specifika säkerhetsbehov för sin applikation, inklusive potentiella sårbarheter introducerade av temporära filer, och kan behöva implementera ytterligare säkerhetsåtgärder utöver vad dessa funktioner tillhandahåller.

I det bredare programmeringslandskapet kan alternativ såsom lagring i minnet (t.ex. genom att använda dynamiska datastrukturer eller minnesavbildade filer) erbjuda bättre prestanda eller säkerhet för hantering av tillfälliga data. Trots detta förblir fysiska temporära filer ett avgörande verktyg i många scenarier, särskilt för stora datamängder eller när interprocesskommunikation är inblandad.
