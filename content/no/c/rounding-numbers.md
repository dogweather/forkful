---
title:                "Avrunding av tall"
date:                  2024-01-26T03:42:55.476143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Avrunding av tall er å kutte av sifrene etter et visst punkt mens man eventuelt justerer det siste beholdte sifferet. Programmerere avrunder for å redusere presisjon når eksakte verdier ikke er nødvendige, håndtere feil med flyttall, eller forberede tall for brukervennlig visning.

## Hvordan:
I C vil du typisk bruke `floor()`, `ceil()`, eller `round()` funksjoner. Her er en rask visning:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Gulv: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Takk: 4.00
    printf("Round: %.2f\n", num_round); // Avrundet: 3.00
    return 0;
}
```

For mer kontroll, som avrunding til et spesifikt sted, ganger du, runder av og deler:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Avrundet til 2 desimaler: %.2f\n", num_rounded); // Avrundet til 2 desimaler: 3.14
```

## Dypdykk
Tilbake i dag, innebar avrunding ofte en manuell prosess—en tung løft med bare penn og papir. Med databehandling automatiserte vi dette, men flyttallaritmetikk brakte med seg nyanser på grunn av dens binære natur, der noen tall ikke kan representeres nøyaktig.

Alternativer til standard avrunding inkluderer trunkering (bare dropp ekstra sifre) eller bankeres avrunding, som runder til det nærmeste partall når det er nøyaktig mellom to verdier, og reduserer bias i gjentatte beregninger.

Implementering blir vanskelig når du trenger å runde av tall med vilkårlig presisjon eller håndtere spesielle tilfeller som uendelighet, signaliserende NaNs, eller subnormale verdier. C-standardsbibliotekfunksjonene håndterer det grunnleggende, men hvis du trenger å runde av desimaler på egendefinerte måter, trenger du mer enn `math.h`.

## Se Også
- [`<math.h>` dokumentasjon](https://en.cppreference.com/w/c/numeric/math)
- [Flyttallaritmetikk](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [Fallgruvene ved verifisering av flyttallberegninger](https://dl.acm.org/doi/10.1145/1186736.1186737)
