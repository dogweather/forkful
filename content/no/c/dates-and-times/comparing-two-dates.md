---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:53:38.979771-07:00
description: "\xC5 sammenligne to datoer i C inneb\xE6rer \xE5 bestemme det kronologiske\
  \ forholdet mellom dem - om \xE9n dato kommer f\xF8r den andre eller om de er like.\
  \ Denne evnen\u2026"
lastmod: '2024-03-11T00:14:14.895850-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sammenligne to datoer i C inneb\xE6rer \xE5 bestemme det kronologiske\
  \ forholdet mellom dem - om \xE9n dato kommer f\xF8r den andre eller om de er like.\
  \ Denne evnen\u2026"
title: Sammenligne to datoer
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sammenligne to datoer i C innebærer å bestemme det kronologiske forholdet mellom dem - om én dato kommer før den andre eller om de er like. Denne evnen er avgjørende i applikasjoner som håndterer planlegging, frister eller føring av poster, da den muliggjør organisering og manipulering av tidsfølsomme data.

## Hvordan gjøres det:

C har ikke en innebygd type for datoer, noe som nødvendiggjør bruken av `time.h`-biblioteket for å jobbe med datums- og tidsstrukturer. `tm`-strukturen og `difftime()`-funksjonen brukes ofte for å sammenligne datoer. Nedenfor er et eksempel som viser hvordan man sammenligner to datoer:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double sekunder;

    // Første dato (ÅÅÅÅ, MM, DD)
    date1.tm_year = 2023 - 1900; // År siden 1900
    date1.tm_mon = 3 - 1;        // Måned [0-11]
    date1.tm_mday = 15;          // Dagen i måneden [1-31]

    // Andre dato (ÅÅÅÅ, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Konverter til time_t format
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Sammenligne
    sekunder = difftime(time1, time2);

    if (sekunder == 0) {
        printf("Datoene er like.\n");
    } else if (sekunder > 0) {
        printf("Første dato kommer etter den andre datoen.\n");
    } else {
        printf("Første dato kommer før den andre datoen.\n");
    }

    return 0;
}
```

Output kunne være:

```text
Første dato kommer før den andre datoen.
```

Dette programmet initialiserer to `tm`-strukturer med spesifikke datoer, konverterer dem til `time_t`-format ved hjelp av `mktime()`, og sammenligner dem til slutt ved hjelp av `difftime()`, som returnerer forskjellen i sekunder (som en `double`) mellom de to tidspunktene.

## Dypdykk

I de tidlige dagene av C krevde dato- og tidsoperasjoner manuelle beregninger, ofte under hensyn til skuddår, det varierende antall dager i månedene, og til og med skuddsekunder. Innføringen av `time.h` i ANSI C-standarden brakte standardisering til håndtering av tid i C, noe som forenklet operasjoner med dato og tid.

Bruk av `time.h` for datoomparasjon er greit, men har begrensninger. `tm`-strukturen tar ikke hensyn til tidssoner eller sommertid, og `difftime()` gir kun forskjellen i sekunder, og mangler finere granularitet for visse applikasjoner.

For applikasjoner som krever mer robuste dato-tidsoperasjoner, inkludert støtte for tidssoner, overganger for sommertid, og mer presise tidsintervaller, tilbyr biblioteker som `date.h` (et Howard Hinnant-datobibliotek, ikke en del av standardbiblioteket) et moderne alternativ til `time.h`. Disse bibliotekene gir mer omfattende verktøy for dato-tidsmanipulering i C++, og drar nytte av tiår med evolusjon i programmeringsspråkdesign. For C-programmerere forblir det nødvendig å benytte seg av disse eksterne bibliotekene eller nøye håndtere detaljene i dato-tidsberegninger direkte for å oppnå presis og kulturelt bevisst dato-tidsmanipulering.
