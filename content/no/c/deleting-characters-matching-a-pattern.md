---
title:                "Slette tegn som matcher et mønster"
date:                  2024-01-20T17:41:52.999628-07:00
model:                 gpt-4-1106-preview
simple_title:         "Slette tegn som matcher et mønster"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sletting av tegn etter mønster er å fjerne spesifikke tegn fra en streng som matcher et gitt kriterium. Programmerere gjør dette for å rense data, forberede tekst for behandling eller for å formatere utdata.

## Slik gjør du:
```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *src = str, *dst = str;
    while (*src) {
        const char *temp_pat = pattern;
        while (*temp_pat && *src != *temp_pat) 
            ++temp_pat;
        if (!(*temp_pat))
            *dst++ = *src;
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "Hei, Norge! 123";
    delete_pattern(str, "123");
    printf("Resultat: %s\n", str); // Skriver ut: Hei, Norge! 
}
```

## Deep Dive
Tidligere ble karakterbehandling ofte gjort med funksjoner som `strtok()` og `strspn()`. Fordelen med vår funksjon `delete_pattern()` er at den er direkte og lett å tilpasse. 

Under panseret jobber vi direkte med pekere. Vi beveger ikke faktisk tegnene fysisk; vi flytter bare hva pekerne 'src' og 'dst' peker på. Dette holder ting raskt og effektivt.

Som alternativ til egenimplementasjon kan regulære uttrykk via biblioteket `regex.h` brukes. Men for mange oppgaver er en enkel funksjon som `delete_pattern` alt du trenger.

Historisk sett har håndtering av strenger i C vært en kilde til mange bugs. Det å skrive sikker og effektiv kode krever forståelse av både minnebehandling og pekeraritmetikk.

## Se Også
- `strtok()` og `strspn()` i C-biblioteket for inndeling og søk i strenger.
- POSIX biblioteket 'regex.h' for regulære uttrykk.
- Online C-standard dokumentasjon for dypere innsikt i funksjonen til biblioteksfunksjonene: http://www.open-std.org/jtc1/sc22/wg14/
