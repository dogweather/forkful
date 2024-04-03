---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:34.093210-07:00
description: "Hvordan: Avrunding av tall i C kan utf\xF8res ved hjelp av ulike funksjoner,\
  \ men den vanligste tiln\xE6rmingen involverer funksjonene `floor()`, `ceil()`,\
  \ og\u2026"
lastmod: '2024-03-13T22:44:41.266023-06:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall i C kan utf\xF8res ved hjelp av ulike funksjoner, men\
  \ den vanligste tiln\xE6rmingen involverer funksjonene `floor()`, `ceil()`, og `round()`."
title: Avrunding av tall
weight: 13
---

## Hvordan:
Avrunding av tall i C kan utføres ved hjelp av ulike funksjoner, men den vanligste tilnærmingen involverer funksjonene `floor()`, `ceil()`, og `round()`. Disse funksjonene er en del av standard mattebiblioteket, så du må inkludere `math.h` i programmet ditt.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Bruker floor() for å avrunde ned
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Bruker ceil() for å avrunde opp
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Bruker round() for å avrunde til nærmeste heltall
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Avrunding til et spesifisert antall desimalplasser innebærer multiplikasjon og divisjon
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Avrunding til to desimalplasser: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Output:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Avrunding til to desimalplasser: 9.53
```

## Dypdykk
Avrunding av tall har dype historiske røtter i matematikk og databehandling, integral både til teoretiske og anvendte aspekter. I C, mens `floor()`, `ceil()`, og `round()` tilbyr grunnleggende funksjonalitet, er essensen av avrunding av flyttall til heltall eller bestemte desimalplasser mer nyansert på grunn av den binære representasjonen av flyttall. Denne representasjonen kan føre til uventede resultater på grunn av hvordan tall som ikke kan representeres nøyaktig i binær form (som 0.1) håndteres.

Disse funksjonene er en del av C-standardbiblioteket, definert i `<math.h>`. Når man avrunder tall, spesielt for finansielle eller presise tekniske beregninger, må man vurdere implikasjonene av å bruke binære flyttall. Alternativer til de innebygde C-funksjonene for svært nøyaktig eller desimal-spesifikk avrunding kan inkludere å implementere tilpassede avrundingsfunksjoner eller å bruke biblioteker designet for vilkårlig-presisjons aritmetikk, som GMP eller MPFR, selv om disse introduserer ekstra kompleksitet og avhengigheter.

I praksis innebærer valg av den riktige tilnærmingen til avrunding i C å balansere behovet for presisjon, ytelse, og praktiskhet, med en skarp forståelse av de domenespesifikke kravene til applikasjonen som utvikles.
