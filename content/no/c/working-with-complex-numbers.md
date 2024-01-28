---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:37:49.028524-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall, en blanding av reelle og imaginære deler (som 3 + 4i), er nøkkelen i avanserte beregninger, som signalbehandling eller løsning av visse ligninger. Programmerere håndterer dem for matematikktunge applikasjoner der tradisjonelle tall ikke strekker til.

## Hvordan:
C, siden C99, har en innebygd kompleks type og bibliotek. Her er hvordan du bruker det:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Erklærer to komplekse tall
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operasjoner med komplekse tall
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Skriver ut resultatene
    printf("Sum: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Produkt: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Absoluttverdi & fasevinkel
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Eksempel på utdata:
```
Sum: 3.0 + 1.0i
Produkt: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Dypdykk
Komplekse tall går flere århundrer tilbake, med røtter i algebra fra det 16. århundre. Frem til nå, de er nå en fast del i mange programmeringsspråk, ikke bare C.

C99-standarden introduserte `<complex.h>`, en header som definerer makroer, funksjoner og `complex` datatypen. Det finnes alternativer - som å lage din egen struktur, men hvorfor finne opp hjulet på nytt? C-standardbiblioteket er optimalisert og klart til bruk.

Til tross for sin kraft, er Cs støtte for komplekse tall ikke uten kritikere. Det kan være mindre intuitivt enn lignende funksjoner i språk som Python, og å håndtere hjørnetilfeller kan bli vanskelig. Men for ren ytelse, er det fortsatt et solid valg.

## Se også
- C99 Standarddokumentasjon for `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- IEEE Standard for Flyttallaritmetikk (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Nettbasert opplæring for matematikk med komplekse tall i C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
