---
title:                "Bruke associative tabeller"
date:                  2024-01-30T19:10:08.733614-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"

category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, eller hash-kart, er nøkkel-verdipar som lar deg lagre og hente data med en nøkkel. De er utrolig nyttige i C fordi de muliggjør raskere dataaksess sammenlignet med lister, spesielt når du håndterer store mengder data.

## Hvordan:

C støtter ikke assosiative tabeller innebygd som noen andre språk, men vi kan bruke strukturer og noen bibliotekfunksjoner for å få lignende funksjonalitet. Her er en enkel implementering som bruker `uthash`-biblioteket, som du må inkludere i prosjektet ditt.

Først, definer en struktur for å holde nøkkel-verdiparene dine:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Dette vil være nøkkelen vår
    char name[10]; // Dette er verdien assosiert med nøkkelen vår
    UT_hash_handle hh; // Gjør denne strukturen hashbar
} person;
```

Deretter, la oss legge til noen oppføringer og hente dem:

```C
int main() {
    person *mine_personer = NULL, *s;

    // Legge til en oppføring
    s = (person*)malloc(sizeof(person));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(mine_personer, id, s);

    // Hente en oppføring
    int bruker_id = 1;
    HASH_FIND_INT(mine_personer, &bruker_id, s);
    if (s) {
        printf("Fant: %s\n", s->name);
    }
    
    return 0;
}
```

Eksempel på utdata ville være:

```
Fant: Alice
```

Ikke glem å frigjøre allokert minne og avvikle hash-tabellen når du er ferdig for å unngå minnelekkasjer.

## Dypdykk

Selv om assosiative tabeller ikke er innebygd i C, fyller biblioteker som `uthash` gapet ganske bra, og gir en ganske rett frem måte å bruke denne funksjonaliteten på. Historisk sett måtte C-utviklere implementere sine versjoner av disse datastrukturene, noe som førte til varierte og ofte komplekse implementeringer, spesielt for dem som nettopp har begynt med språket.

Husk, effektiviteten av å bruke assosiative tabeller i C avhenger sterkt av hvor godt hash-funksjonen distribuerer verdier over tabellen for å minimere kollisjoner. Mens biblioteker som `uthash` tilbyr en god balanse mellom brukervennlighet og ytelse, i kritiske applikasjoner hvor ytelsen er paramount, kan det hende du vil tilpasse eller implementere din egen hash-tabell.

For applikasjoner som krever maksimal effektivitet, kan alternative datastrukturer eller til og med andre programmeringsspråk med innebygd støtte for assosiative tabeller være et bedre valg. Derimot, for mange situasjoner, spesielt hvor du allerede arbeider inne i et C-miljø, gir bruk av et bibliotek som `uthash` en praktisk balanse mellom ytelse og bekvemmelighet.
