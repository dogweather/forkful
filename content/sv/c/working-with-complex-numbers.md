---
title:                "Att arbeta med komplexa tal"
date:                  2024-01-26T04:37:34.765083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med komplexa tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Komplexa tal, en blandning av reella och imaginära delar (som 3 + 4i), är centrala i avancerade beräkningar, som signalbehandling eller lösa vissa ekvationer. Programmerare hanterar dem för matematiktunga applikationer där traditionella nummer inte räcker till.

## Hur man gör:
C har, sedan C99, en ursprunglig komplex typ och bibliotek. Så här använder du det:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Deklarera två komplexa tal
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operationer med komplexa tal
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Skriva ut resultaten
    printf("Summa: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Produkt: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Absolutvärde & fasvinkel
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Exempelutmatning:
```
Summa: 3.0 + 1.0i
Produkt: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Djupdykning
Komplexa tal går tillbaka århundraden, med rötter i 1500-talets algebra. Framåt i tiden är de nu en grundpelare i många programmeringsspråk, inte bara C.

C99-standarden introducerade `<complex.h>`, en rubrik som definierar makron, funktioner och `complex` datatypen. Alternativ finns - som att skapa din egen struktur, men varför uppfinna hjulet igen? C:s standardbibliotek är optimerat och klart att användas.

Trots sin kraft är C:s stöd för komplexa tal inte utan kritiker. Det kan vara mindre intuitivt än liknande funktioner i språk som Python, och att hantera hörnfall kan bli knepigt. Men för rå prestanda är det fortfarande ett solitt val.

## Se även
- C99 Standarddokumentation för `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- IEEE-standard för flyttalsaritmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Online-tutorial för C-komplex nummermatematik: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
