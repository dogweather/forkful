---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:53.473297-07:00
description: "Hvordan: I C st\xF8ttes komplekse tall av Standardbiblioteket, spesifikt\
  \ `<complex.h>`. For \xE5 bruke dem, erkl\xE6r variabler med typen `double complex`\
  \ (eller\u2026"
lastmod: '2024-03-13T22:44:41.264745-06:00'
model: gpt-4-0125-preview
summary: "I C st\xF8ttes komplekse tall av Standardbiblioteket, spesifikt `<complex.h>`."
title: "\xC5 Arbeide med Komplekse Tall"
weight: 14
---

## Hvordan:
I C støttes komplekse tall av Standardbiblioteket, spesifikt `<complex.h>`. For å bruke dem, erklær variabler med typen `double complex` (eller `float complex` for enkel presisjon). Her er hvordan man utfører grunnleggende operasjoner:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Erklærer et komplekst tall 1+2i
    double complex z2 = 1.0 - 2.0*I; // Erklærer et annet komplekst tall 1-2i
    
    // Addisjon
    double complex sum = z1 + z2;
    printf("Sum: %.2f + %.2fi\n", creal(sum), cimag(sum)); // Utdata: Sum: 2.00 + 0.00i

    // Multiplikasjon
    double complex produkt = z1 * z2;
    printf("Produkt: %.2f + %.2fi\n", creal(produkt), cimag(produkt)); // Utdata: Produkt: 5.00 + 0.00i

    // Kompleks konjugert
    double complex konjugert = conj(z1);
    printf("Konjugert av z1: %.2f + %.2fi\n", creal(konjugert), cimag(konjugert)); // Utdata: Konjugert av z1: 1.00 - 2.00i
    
    // Størrelse
    double størrelse = cabs(z1);
    printf("Størrelsen av z1: %.2f\n", størrelse); // Utdata: Størrelsen av z1: 2.24

    // Fase
    double fase = carg(z1);
    printf("Fase av z1: %.2f\n", fase); // Utdata i radianer
    
    return 0;
}
```
Merk at `I` er en konstant som representerer den imaginære enheten i `<complex.h>`. Funksjoner som `creal()` og `cimag()` trekker ut henholdsvis reelle og imaginære deler, mens `conj()` beregner det komplekse konjugatet. For størrelsen og fasen (argumentet) til komplekse tall, brukes `cabs()` og `carg()`.

## Dypdykk
Støtten for komplekse tall i C er relativt nylig, med standardisering i C99. Før dette var aritmetikk med komplekse tall i C tungvint, ofte kreves det egendefinerte datastrukturer og funksjoner. Inkluderingen av `<complex.h>` og de komplekse datatypene ga et betydelig løft til språkets evner for vitenskapelige og ingeniørrelaterte applikasjoner. Det er imidlertid verdt å merke seg at noen språk, som Python, tilbyr mer intuitiv støtte for komplekse tall gjennom innebygde datatyper og et rikere sett med biblioteksfunksjoner. Til tross for dette gjør ytelsen og kontrollen som C tilbyr, det til et foretrukket valg for oppgaver med høy ytelse, selv om det betyr å håndtere en noe mer omstendelig syntaks for kompleks aritmetikk.
