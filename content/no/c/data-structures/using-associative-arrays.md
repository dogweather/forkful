---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:42.892958-07:00
description: "Assosiative tabeller, kjent i andre spr\xE5k som kart eller ordb\xF8\
  ker, er n\xF8kkel-verdi-par som brukes til effektiv dataoppslag og -manipulasjon.\
  \ I motsetning\u2026"
lastmod: '2024-02-25T18:49:39.441509-07:00'
model: gpt-4-0125-preview
summary: "Assosiative tabeller, kjent i andre spr\xE5k som kart eller ordb\xF8ker,\
  \ er n\xF8kkel-verdi-par som brukes til effektiv dataoppslag og -manipulasjon. I\
  \ motsetning\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent i andre språk som kart eller ordbøker, er nøkkel-verdi-par som brukes til effektiv dataoppslag og -manipulasjon. I motsetning til tradisjonelle tabeller som bruker heltallsindekser, bruker assosiative tabeller nøkler, noe som gjør datainngang mer intuitivt og fleksibelt for programmerere.

## Hvordan:

C har ikke innebygd støtte for assosiative tabeller som noen høyere nivå språk, men du kan simulere dem ved å bruke strukturer og hashing. Nedenfor er et forenklet eksempel som bruker en kombinasjon av en struktur og en enkel hashfunksjon for å implementere en assosiativ tabell for lagring og tilgang til heltall ved strengnøkler.

Først, definer en struktur for å representere et enkelt nøkkel-verdi-par og en annen for å representere den assosiative tabellen selv:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePar;

typedef struct {
    KeyValuePar* items[TABLE_SIZE];
} AssosTabell;

unsigned int hash(char* key) {
    unsigned long int verdi = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        verdi = verdi * 37 + key[i];
    }

    verdi = verdi % TABLE_SIZE;

    return verdi;
}

void initArray(AssosTabell* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssosTabell* array, char* key, int value) {
    unsigned int slot = hash(key);

    KeyValuePar* item = (KeyValuePar*)malloc(sizeof(KeyValuePar));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssosTabell* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssosTabell a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Utdata: 1
    printf("%d\n", find(&a, "key2")); // Utdata: 2

    return 0;
}
```

Dette eksempelet demonstrerer grunnleggende operasjoner: initialisering av en assosiativ tabell, innsetting av nøkkel-verdi-par og finne verdier ved nøkler. Merk at denne koden mangler kollisjonshåndtering og er ment for pedagogiske formål.

## Dypdykk

Konseptet med assosiative tabeller er eldre enn C, men språkets lavnivånatur støtter dem ikke direkte som innebygde typer. Dette oppfordrer til en dypere forståelse av datastrukturer og algoritmer, inkludert hashingsmekanismer for effektiv nøkkel-verdi-mapping. Mange C-biblioteker og rammeverk tilbyr mer sofistikerte tilnærminger for å implementere assosiative tabeller, som GLib's `GHashTable`, som gir en robust implementering komplett med kollisjonshåndtering, dynamisk størrelseendring og støtte for vilkårlige nøkkel- og verdi-typer.

Selv om manuell konstruksjon av assosiative tabeller i C kan sees på som omfattende sammenlignet med språk med innebygd støtte, tilbyr det uvurderlig innsikt i indre arbeid av datastrukturer, skjerping av en programmerers ferdigheter i problemløsning og optimalisering. Men for produksjonskode eller mer komplekse applikasjoner, er det ofte en mer praktisk og tidsbesparende tilnærming å utnytte eksisterende biblioteker som GLib.
