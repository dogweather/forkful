---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:41.076417-07:00
description: "Att skriva tester i C inneb\xE4r att skapa mindre, hj\xE4lpprogram eller\
  \ funktioner som automatiskt verifierar din kods funktionalitet. Programmerare g\xF6\
  r detta\u2026"
lastmod: '2024-03-13T22:44:38.386498-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i C inneb\xE4r att skapa mindre, hj\xE4lpprogram eller\
  \ funktioner som automatiskt verifierar din kods funktionalitet."
title: Skriva tester
weight: 36
---

## Hur:
Även om C inte har ett inbyggt testramverk som vissa andra språk, kan du fortfarande skriva effektiva tester genom att använda assert.h för enkla påståenden eller integrera tredjepartsramverk som CUnit eller Unity för mer strukturerade tester. Här är ett grundläggande exempel som använder assert.h för att testa en funktion som lägger till två heltal:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("Alla additionstester har gått igenom.\n");
}

int main() {
    test_addition();
    return 0;
}
```

I `my_math.h` kan du ha:

```c
// Enkel adderingsfunktion
int add(int a, int b) {
    return a + b;
}
```

Att köra testfunktionen i din `main` funktion ger utskriften:

```
Alla additionstester har gått igenom.
```

För en mer omfattande testuppsättning med hjälp av ett ramverk som Unity skulle du integrera ramverket i ditt projekt och sedan skriva testfall på liknande sätt, men använd ramverkets API för påståenden och testkörning.

## Fördjupning
Testning i C har historiskt sett varit en manuell och något ad hoc process på grund av språkets lågnivånatur och bristen på ett standardiserat testramverk. Denna manuella metod ledde ofta till mindre grundliga testningspraxis jämfört med språk med inbyggt teststöd. Eftersom C-språket varit avgörande för utvecklingen av grundläggande mjukvarusystem, ledde denna brist på formella testramverk till att C-gemenskapen utvecklade tredjepartslösningar, som CUnit och Unity.

Dessa verktyg, trots att de är externa till det standardiserade C-biblioteket, erbjuder funktionalitet liknande testramverk i andra språk, och ger ett strukturerat sätt att definiera, köra och utvärdera tester. De hjälper till att överbrygga gapet mellan Cs kraftfulla systemnivååtkomst och den moderna utvecklingspraxisen med automatiserad testning. Det är värt att notera att medan dessa verktyg i hög grad förbättrar testprocessen i C, kan de introducera en inlärningskurva och öka komplexiteten i projektuppsättningen jämfört med språk med integrerat teststöd. Således är investeringen i att sätta upp en korrekt testmiljö i C väl motiverad för projekt där tillförlitlighet och underhållbarhet är av yttersta vikt, även med tanke på möjliga alternativ.
