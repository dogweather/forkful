---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:02.503679-07:00
description: "Att logga i C inneb\xE4r att man registrerar fl\xF6det och betydande\
  \ h\xE4ndelser i ett program under dess k\xF6rtid, vilket ger en konkret \xF6versikt\
  \ av dess beteende\u2026"
lastmod: '2024-03-13T22:44:38.389608-06:00'
model: gpt-4-0125-preview
summary: "Att logga i C inneb\xE4r att man registrerar fl\xF6det och betydande h\xE4\
  ndelser i ett program under dess k\xF6rtid, vilket ger en konkret \xF6versikt av\
  \ dess beteende\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?

Att logga i C innebär att man registrerar flödet och betydande händelser i ett program under dess körtid, vilket ger en konkret översikt av dess beteende och prestanda. Programmerare använder loggning för felsökning, övervakning av mjukvaruhälsa och för att säkerställa systemets säkerhet.

## Hur gör man:

I C kan loggning uppnås med grundläggande filoperationer eller genom att använda mer sofistikerade bibliotek. För enkelhetens skull startar vi med standard I/O-biblioteket. Följande kodsnuttar visar grundläggande implementationer av loggning.

För att logga enkla meddelanden:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Öppna loggfilen i lägga till-läge
    
    if (logFile == NULL) {
        perror("Fel vid öppning av loggfil.");
        return -1;
    }
    
    fprintf(logFile, "Startar applikationen.\n");
    
    // Din applikationslogik här
    
    fprintf(logFile, "Applikationen avslutades framgångsrikt.\n");
    fclose(logFile);
    
    return 0;
}
```

Utdata i `application.log`:

```
Startar applikationen.
Applikationen avslutades framgångsrikt.
```

För att inkludera mer detaljerade loggar med tidsstämplar och loggnivåer:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Ta bort radbrytningstecken
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Fel vid öppning av loggfil.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Applikationen startar");
    // Din applikationslogik här
    logMessage(logFile, "ERROR", "Ett exempelfel");
    
    fclose(logFile);
    
    return 0;
}
```

Utdata i `detailed.log`:

```
[Tor Mar 10 14:32:01 2023] INFO - Applikationen startar
[Tor Mar 10 14:32:02 2023] ERROR - Ett exempelfel
```

## Djupdykning

Som demonstrerat, beror loggning i C på enkla filoperationer, vilket är effektivt men inte lika kraftfullt eller flexibelt som loggningsverktyg i andra språk, såsom Pythons `logging`-modul eller Javas `Log4j`. För mer avancerade loggningsmöjligheter i C vänder sig utvecklare ofta till bibliotek som `syslog` på Unix-liknande system, vilket ger systemomfattande logghantering, eller tredjepartsbibliotek som `log4c`.

Historiskt sett har loggning varit en integrerad del av programmering, vilket går tillbaka till tidiga programmeringspraxis där spårning och förståelse för programflödet och fel gjordes främst genom fysiska utskrifter. När systemen utvecklades blev loggningen mer sofistikerad, och stöder nu olika allvarlighetsnivåer, loggrotation och asynkron loggning.

Även om C:s standardbibliotek tillhandahåller de grundläggande verktygen för att implementera loggning, leder dess begränsningar ofta till skapandet av anpassade loggningsramverk eller användning av externa bibliotek för mer funktionsrika och flexibla loggningslösningar. Trots dessa begränsningar är förståelsen och implementeringen av grundläggande loggning i C avgörande för felsökning och underhåll av mjukvara, särskilt i miljöer där externa beroenden ska minimeras.
