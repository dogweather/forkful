---
title:                "Werken met complexe getallen"
aliases: - /nl/c/working-with-complex-numbers.md
date:                  2024-02-03T18:13:58.428380-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-complex-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Complexe getallen bestaan uit een reëel deel en een imaginair deel, voorgesteld als `a + bi` waarbij `i` de vierkantswortel van `-1` is. Programmeurs werken met complexe getallen in verschillende velden zoals elektrotechniek, kwantumcomputing, en vloeistofdynamica, waarbij ze hun unieke eigenschappen benutten voor simulaties, signaalverwerking, en het oplossen van specifieke soorten wiskundige vergelijkingen.

## Hoe:

In C worden complexe getallen ondersteund door de Standaard Bibliotheek, specifiek `<complex.h>`. Om ze te gebruiken, verklaar je variabelen met het type `double complex` (of `float complex` voor enkele precisie). Hier is hoe je basisoperaties uitvoert:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Verklaar een complex getal 1+2i
    double complex z2 = 1.0 - 2.0*I; // Verklaar een ander complex getal 1-2i
    
    // Optelling
    double complex som = z1 + z2;
    printf("Som: %.2f + %.2fi\n", creal(som), cimag(som)); // Output: Som: 2.00 + 0.00i

    // Vermenigvuldiging
    double complex product = z1 * z2;
    printf("Product: %.2f + %.2fi\n", creal(product), cimag(product)); // Output: Product: 5.00 + 0.00i

    // Complex Toegevoegd
    double complex toegevoegd = conj(z1);
    printf("Toegevoegd van z1: %.2f + %.2fi\n", creal(toegevoegd), cimag(toegevoegd)); // Output: Toegevoegd van z1: 1.00 - 2.00i
    
    // Magnitude
    double magnitude = cabs(z1);
    printf("Magnitude van z1: %.2f\n", magnitude); // Output: Magnitude van z1: 2.24

    // Fase
    double fase = carg(z1);
    printf("Fase van z1: %.2f\n", fase); // Output in radialen
    
    return 0;
}
```
Merk op dat `I` een constante is die de imaginaire eenheid in `<complex.h>` vertegenwoordigt. Functies zoals `creal()` en `cimag()` extraheren respectievelijk de reële en imaginaire delen, terwijl `conj()` het complex toegevoegde berekent. Voor de magnitude en fase (argument) van complexe getallen worden `cabs()` en `carg()` gebruikt.

## Diepgaand

De ondersteuning voor complexe getallen in C is relatief recent, omdat het gestandaardiseerd werd in C99. Voor deze tijd was rekenen met complexe getallen in C omslachtig, waarbij vaak aangepaste gegevensstructuren en functies nodig waren. De opname van `<complex.h>` en de complexe datatypes leverde een aanzienlijke verbetering op voor de mogelijkheden van de taal voor wetenschappelijke en technische toepassingen. Het is echter vermeldenswaard dat sommige talen, zoals Python, intuitievere ondersteuning bieden voor complexe getallen door ingebouwde datatypes en een rijkere set bibliotheekfuncties. Ondanks dit blijft de prestatie en controle die C biedt een voorkeurskeuze voor taken op het gebied van high-performance computing, zelfs als dat betekent omgaan met een iets uitgebreidere syntax voor complexe rekenkunde.
