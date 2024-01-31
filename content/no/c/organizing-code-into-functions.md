---
title:                "Organisering av kode i funksjoner"
date:                  2024-01-26T01:09:37.068122-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisering av kode i funksjoner"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å organisere kode i funksjoner handler om å bryte ned koden i gjenbrukbare blokker som utfører spesifikke oppgaver. Det gjør koden enklere å lese, feilsøke og vedlikeholde.

## Hvordan:
La oss ta et enkelt eksempel: si at du vil legge sammen to tall flere ganger.

Uten funksjoner:
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("Sum1: %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("Sum2: %d\n", sum2);
    
    // Flere addisjoner her...
    
    return 0;
}
```

Med funksjoner:
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = add(5, 3);
    printf("Sum1: %d\n", sum1);
    
    int sum2 = add(2, 8);
    printf("Sum2: %d\n", sum2);
    
    // Bruk add()-funksjonen for flere addisjoner...
    
    return 0;
}
```

Utdata:
```
Sum1: 8
Sum2: 10
```

## Dypdykk
Før C hadde funksjoner, ble programmering ofte gjort lineært, mye som en oppskrift. Men da programmene vokste, ble kode-duplisering et problem. Funksjoner var løsningen - de tillot oss å kjøre samme kodeblokk fra forskjellige deler av et program uten å omskrive det hver gang. Dette sparer ikke bare plass, men også tid når man oppdaterer: endre funksjonen på ett sted, og hver del av koden din som bruker den får oppdateringen.

Alternativer til funksjoner kan inkludere innebygd kode ("inline code"), makroer eller kopier-og-lim- programmering ("copy-and-paste coding"), men dette kan føre til oppblåst, feilutsatt og vanskelig å vedlikeholde kode. Funksjoner, derimot, kapsler inn funksjonalitet, definerer klare grensesnitt, og kan redusere sideeffekter med riktig bruk av rekkevidde ("scope").

Når du implementerer funksjoner, bør du vurdere et par detaljer: for det første, prøv å få dem til å gjøre bare én ting - dette er kjent som "Single Responsibility Principle". For det andre er navn viktig - velg beskrivende navn for funksjoner og deres parametere for å gjøre koden selv-dokumenterende.

## Se Også
For mer om funksjoner i C, ta en titt på disse:

- C Standard Library referanse: https://en.cppreference.com/w/c/header
- C Programming: A Modern Approach av K.N. King: En bok med en dypdykk på funksjoner.
- Learn-C.org: Seksjon om funksjoner: https://www.learn-c.org/en/Functions
