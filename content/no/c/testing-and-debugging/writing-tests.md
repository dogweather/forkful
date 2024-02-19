---
aliases:
- /no/c/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:19.432774-07:00
description: "\xC5 skrive tester i C inneb\xE6rer \xE5 lage mindre, hjelpeprogrammer\
  \ eller funksjoner som automatisk verifiserer funksjonaliteten til koden din. Programmerere\u2026"
lastmod: 2024-02-18 23:08:54.398014
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i C inneb\xE6rer \xE5 lage mindre, hjelpeprogrammer eller\
  \ funksjoner som automatisk verifiserer funksjonaliteten til koden din. Programmerere\u2026"
title: Skrive tester
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive tester i C innebærer å lage mindre, hjelpeprogrammer eller funksjoner som automatisk verifiserer funksjonaliteten til koden din. Programmerere gjør dette for å sikre at programvaren deres fungerer som forventet, for å oppdage feil tidlig, og for å lette fremtidige kodeendringer uten uønskede bivirkninger.

## Hvordan:
Selv om C ikke har et innebygget testrammeverk som noen andre språk, kan du fortsatt skrive effektive tester ved hjelp av assert.h for enkle påstander eller integrere tredjepartsrammeverk som CUnit eller Unity for mer strukturert testing. Her er et grunnleggende eksempel som bruker assert.h for å teste en funksjon som legger sammen to heltall:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("Alle addisjonstester bestått.\n");
}

int main() {
    test_addition();
    return 0;
}
```

I `my_math.h`, kan du ha:

```c
// Enkel addisjonsfunksjon
int add(int a, int b) {
    return a + b;
}
```

Å kjøre testfunksjonen i din `main` funksjon gir utskriften:

```
Alle addisjonstester bestått.
```

For en mer omfattende testoppsett ved bruk av et rammeverk som Unity, ville du inkorporert rammeverket i prosjektet ditt, og deretter skrevet testtilfeller på lignende måte, men ved å bruke rammeverkets API for påstander og testkjøring.

## Dypdykk
Testing i C har historisk sett vært en manuell og noe ad hoc prosess på grunn av språkets lavnivå natur og mangel på et standardisert testrammeverk. Denne manuelle tilnærmingen førte ofte til mindre grundige testpraksiser sammenlignet med språk med innebygd teststøtte. Siden C-språket har vært avgjørende i utviklingen av grunnleggende programvaresystemer, har denne mangelen på formelle testrammeverk oppmuntret C-samfunnet til å utvikle tredjepartsløsninger, som CUnit og Unity.

Disse verktøyene, selv om de er eksterne til det standard C-biblioteket, tilbyr funksjonalitet lik testrammeverk i andre språk, og tilbyr en strukturert måte å definere, kjøre og evaluere tester på. De hjelper med å bro over gapet mellom Cs kraftfulle systemnivå tilgang og den moderne utviklingspraksisen med automatisert testing. Det er verdt å merke seg at selv om disse verktøyene i stor grad forbedrer testprosessen i C, kan de introdusere en læringskurve og øke kompleksiteten til prosjektoppsett sammenlignet med språk med integrert teststøtte. Derfor er det godt begrunnet å investere i oppsettet av et ordentlig testmiljø i C for prosjekter der pålitelighet og vedlikeholdbarhet er av største viktighet, selv i lys av mulige alternativer.
