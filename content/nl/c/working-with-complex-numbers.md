---
title:                "Werken met complexe getallen"
date:                  2024-01-28T22:12:23.741241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen, een mix van reële en imaginaire delen (zoals 3 + 4i), zijn essentieel in geavanceerde berekeningen, zoals signaalverwerking of het oplossen van bepaalde vergelijkingen. Programmeurs gebruiken ze voor toepassingen zwaar leunend op wiskunde waar traditionele getallen niet voldoen.

## Hoe:
C, sinds C99, heeft een native complex type en bibliotheek. Hier is hoe je het gebruikt:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Declareer twee complexe getallen
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operaties met complexe getallen
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Het afdrukken van de resultaten
    printf("Som: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Product: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Absolute waarde & fasehoek
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Voorbeelduitvoer:
```
Som: 3.0 + 1.0i
Product: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Diepere Duik
Complexe getallen gaan eeuwen terug, met wortels in de algebra van de 16e eeuw. In de loop der tijd zijn ze nu een vast onderdeel in vele programmeertalen, niet alleen in C.

De C99-standaard introduceerde `<complex.h>`, een header die macro's, functies, en het `complex` datatype definieert. Alternatieven bestaan - zoals het creëren van je eigen structuur, maar waarom het wiel opnieuw uitvinden? De C-standaardbibliotheek is geoptimaliseerd en klaar voor gebruik.

Ondanks zijn potentie, is de ondersteuning van complexe getallen in C niet zonder kritiek. Het kan minder intuïtief zijn dan vergelijkbare functies in talen zoals Python, en het omgaan met randgevallen kan lastig zijn. Maar voor rauwe prestaties blijft het een solide keuze.

## Zie Ook
- C99 Standaarddocumentatie voor `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- IEEE Standaard voor drijvende-kommagetallen (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Online tutorial voor complexe getallen rekenkunde in C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
