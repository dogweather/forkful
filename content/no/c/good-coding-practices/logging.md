---
aliases:
- /no/c/logging/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:00.527069-07:00
description: "Logging i C inneb\xE6rer \xE5 registrere flyten og merkbare hendelser\
  \ av et program under kj\xF8retiden, som gir en konkret gjennomgang av dets oppf\xF8\
  rsel og\u2026"
lastmod: 2024-02-18 23:08:54.400967
model: gpt-4-0125-preview
summary: "Logging i C inneb\xE6rer \xE5 registrere flyten og merkbare hendelser av\
  \ et program under kj\xF8retiden, som gir en konkret gjennomgang av dets oppf\xF8\
  rsel og\u2026"
title: Logging
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Logging i C innebærer å registrere flyten og merkbare hendelser av et program under kjøretiden, som gir en konkret gjennomgang av dets oppførsel og ytelse. Programmerere bruker logging til feilsøkingsformål, overvåking av programvarens helse og sikring av systemets sikkerhet.

## Hvordan:

I C kan logging oppnås med grunnleggende filoperasjoner eller ved å bruke mer sofistikerte biblioteker. For enkelhets skyld, starter vi med standard I/U-biblioteket. Følgende kodeeksempler viser grunnleggende implementeringer av logging.

For å logge enkle meldinger:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Åpne loggfila i tilleggsmodus
    
    if (logFile == NULL) {
        perror("Feil ved åpning av loggfil.");
        return -1;
    }
    
    fprintf(logFile, "Starter applikasjon.\n");
    
    // Logikken i applikasjonen din her
    
    fprintf(logFile, "Applikasjonen avsluttet vellykket.\n");
    fclose(logFile);
    
    return 0;
}
```

Utdata i `application.log`:

```
Starter applikasjon.
Applikasjonen avsluttet vellykket.
```

For å inkludere mer detaljerte logger med tidsstempel og loggnivå:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Fjern linjeskifttegnet
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Feil ved åpning av loggfil.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Applikasjon starter");
    // Logikken i applikasjonen din her
    logMessage(logFile, "ERROR", "Et eksempel på feil");
    
    fclose(logFile);
    
    return 0;
}
```

Utdata i `detailed.log`:

```
[Thu Mar 10 14:32:01 2023] INFO - Applikasjon starter
[Thu Mar 10 14:32:02 2023] ERROR - Et eksempel på feil
```

## Dypdykk

Logging i C, som demonstrert, er avhengig av enkle filoperasjoner, noe som er effektivt, men ikke like kraftig eller fleksibelt som loggingfasiliteter i andre språk, som Pythons `logging`-modul eller Javas `Log4j`. For mer avanserte loggeegenskaper i C, vender utviklere seg ofte til biblioteker som `syslog` på Unix-lignende systemer, som gir systemomfattende logghåndtering, eller tredjepartsbiblioteker som `log4c`.

Historisk sett har logging vært en integrert del av programmering, som går tilbake til tidlige programmeringspraksiser der sporing og forståelse av programflyt og feil for det meste ble gjort gjennom fysiske utskrifter. Ettersom systemene utviklet seg, ble logging mer sofistikert, og støtter nå ulike alvorlighetsnivåer, loggrotasjon og asynkron logging.

Selv om Cs standardbibliotek gir grunnleggende verktøy for implementering av logging, fører dets begrensninger ofte til skapingen av egendefinerte loggingsrammeverk eller adopsjon av eksterne biblioteker for mer funksjonsrike og fleksible loggløsninger. Til tross for disse begrensningene, er forståelse og implementering av grunnleggende logging i C avgjørende for feilsøking og vedlikehold av programvare, spesielt i miljøer der eksterne avhengigheter bør minimeres.
