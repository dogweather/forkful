---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:32.246542-07:00
description: "\xC5 slette tegn som samsvarer med et bestemt m\xF8nster fra strenger\
  \ i C handler om \xE5 fjerne alle forekomster av visse tegn som passer forh\xE5\
  ndsdefinerte\u2026"
lastmod: '2024-03-13T22:44:41.253849-06:00'
model: gpt-4-0125-preview
summary: "\xC5 slette tegn som samsvarer med et bestemt m\xF8nster fra strenger i\
  \ C handler om \xE5 fjerne alle forekomster av visse tegn som passer forh\xE5ndsdefinerte\
  \ kriterier."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## Hvordan:
C kommer ikke med en innebygd funksjon for direkte å slette tegn fra en streng basert på et mønster, ulikt noen høyere nivå språk. Imidlertid kan du enkelt oppnå denne oppgaven ved manuelt å iterere over strengen og bygge en ny som ekskluderer de uønskede tegnene. For eksempel, la oss anta at du vil fjerne alle sifre fra en streng. Du kan gjøre dette som følger:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C Programmering 101: Grunnleggende!";
    remove_digits(str);
    printf("Resultat: %s\n", str);
    return 0;
}
```

Eksempel på utdata:
```
Resultat: C Programmering : Grunnleggende!
```

Dette eksempelet utnytter `isdigit` fra `ctype.h` for å identifisere sifre, ved å flytte tegn som ikke er sifre til begynnelsen av strengen og avslutte strengen når alle tegn er evaluert.

## Dypdykk
Løsningen presentert bruker en tilnærming med to pekere innenfor samme array for effektivt å filtrere ut uønskede tegn, en teknikk som er emblematiske for C's praktiske håndtering av minnefilosofi. Denne metoden er effektiv fordi den opererer på stedet, og unngår behovet for ekstra minneallokering og dermed minimerer overflødig.

Historisk sett har fraværet av høynivå strengmanipuleringsfunksjoner i C tvunget programmerere til å utvikle en dyp forståelse av strengbehandling på minnenivå, noe som har ført til nyskapende tilnærminger som den ovenfor. Selv om dette har fordelen av større kontroll og effektivitet, kommer det med en høyere risiko for feil, som bufferoverflyt og feil ved én-offs.

I moderne utviklingskontekster, spesielt de som vektlegger sikkerhet og trygghet, kan språk som abstraherer bort slike lavnivåoperasjoner foretrekkes for oppgaver med strengmanipulering. Likevel forblir forståelsen og bruken av disse C-teknikkene uvurderlig for scenarioer som krever finjustert ytelsesoptimalisering eller for å arbeide innenfor miljøer der C's minimalisme og hastighet er av største viktighet.
