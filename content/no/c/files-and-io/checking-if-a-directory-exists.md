---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:52:44.094051-07:00
description: "Hvordan: I C kan eksistensen av en mappe sjekkes ved \xE5 bruke `stat`-funksjonen,\
  \ som henter informasjon om filen eller mappen p\xE5 en spesifisert bane.\u2026"
lastmod: '2024-03-13T22:44:41.287661-06:00'
model: gpt-4-0125-preview
summary: "I C kan eksistensen av en mappe sjekkes ved \xE5 bruke `stat`-funksjonen,\
  \ som henter informasjon om filen eller mappen p\xE5 en spesifisert bane."
title: Sjekke om en mappe eksisterer
weight: 20
---

## Hvordan:
I C kan eksistensen av en mappe sjekkes ved å bruke `stat`-funksjonen, som henter informasjon om filen eller mappen på en spesifisert bane. Makroen `S_ISDIR` fra `sys/stat.h` brukes deretter for å vurdere om den hentede informasjonen tilsvarer en mappe.

Slik kan du bruke `stat` og `S_ISDIR` for å sjekke om en mappe eksisterer:

```c
#include <stdio.h>
#include <sys/stat.h>

int main() {
    struct stat stats;
    
    // Banen til mappen som skal sjekkes
    char *dirPath = "/path/to/directory";

    // Få statusen til banen
    int resultat = stat(dirPath, &stats);

    // Sjekk om mappen eksisterer
    if (resultat == 0 && S_ISDIR(stats.st_mode)) {
        printf("Mappen eksisterer.\n");
    } else {
        printf("Mappen eksisterer ikke.\n");
    }

    return 0;
}
```

Eksempel på utskrift:
```
Mappen eksisterer.
```

Eller, hvis mappen ikke eksisterer:
```
Mappen eksisterer ikke.
```

## Dypdykk:
`stat`-strukturen og -funksjonen har vært en del av C programmeringsspråket i flere tiår, og stammer fra Unix. De tilbyr en standardisert måte å hente filsysteminformasjon på, som, til tross for å være relativt lavnivå, er mye brukt på grunn av sin enkelhet og direkte tilgang til filsystemets metadata.

Historisk sett har sjekking av eksistensen og egenskapene til filer og mapper med `stat` og dets derivater (som `fstat` og `lstat`) vært en vanlig tilnærming. Men disse funksjonene kommuniserer direkte med OS-kjernen, noe som kan introdusere overhead og potensielle feil hvis de ikke håndteres riktig.

For nye prosjekter eller ved arbeid i høynivå-scenarioer, kan programmerere velge mer abstraherte filhåndteringsmekanismer levert av moderne rammer eller biblioteker som håndterer feil mer nådig og gir et enklere API. Likevel, å forstå og kunne bruke `stat` forblir en verdifull ferdighet for scenarioer som krever direkte manipulasjon av filsystemet, som systemprogrammering eller når man arbeider i begrensede miljøer hvor avhengighet av store biblioteker er upraktisk.
