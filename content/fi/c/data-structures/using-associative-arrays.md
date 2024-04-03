---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:46.620358-07:00
description: "Kuinka: C ei tarjoa sis\xE4\xE4nrakennettua tukea assosiatiivisille\
  \ taulukoille kuten jotkut korkeamman tason kielet, mutta voit simuloida niit\xE4\
  \ k\xE4ytt\xE4m\xE4ll\xE4\u2026"
lastmod: '2024-03-13T22:44:57.031819-06:00'
model: gpt-4-0125-preview
summary: "C ei tarjoa sis\xE4\xE4nrakennettua tukea assosiatiivisille taulukoille\
  \ kuten jotkut korkeamman tason kielet, mutta voit simuloida niit\xE4 k\xE4ytt\xE4\
  m\xE4ll\xE4 rakenteita ja hajautusta."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
C ei tarjoa sisäänrakennettua tukea assosiatiivisille taulukoille kuten jotkut korkeamman tason kielet, mutta voit simuloida niitä käyttämällä rakenteita ja hajautusta. Alla on yksinkertaistettu esimerkki käyttäen yhdistelmää rakenteesta ja yksinkertaisesta hajautusfunktiosta toteuttamaan assosiatiivisen taulukon, joka varastoi ja käyttää kokonaislukuja merkkijonoavaimilla.

Määrittele ensin rakenne, joka edustaa yhtä avain-arvo-paria ja toinen, joka edustaa itse assosiatiivista taulukkoa:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} AvainArvoPari;

typedef struct {
    AvainArvoPari* items[TABLE_SIZE];
} AssosTaulukko;

unsigned int hajautus(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void alustaTaulukko(AssosTaulukko* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void lisaa(AssosTaulukko* array, char* key, int value) {
    unsigned int paikka = hajautus(key);

    AvainArvoPari* item = (AvainArvoPari*)malloc(sizeof(AvainArvoPari));
    item->key = strdup(key);
    item->value = value;

    array->items[paikka] = item;
}

int etsi(AssosTaulukko* array, char* key) {
    unsigned int paikka = hajautus(key);

    if (array->items[paikka]) {
        return array->items[paikka]->value;
    }
    return -1;
}

int main() {
    AssosTaulukko a;
    alustaTaulukko(&a);

    lisaa(&a, "key1", 1);
    lisaa(&a, "key2", 2);

    printf("%d\n", etsi(&a, "key1")); // Tuloste: 1
    printf("%d\n", etsi(&a, "key2")); // Tuloste: 2

    return 0;
}
```

Tämä esimerkki osoittaa perusoperaatiot: assosiatiivisen taulukon alustamisen, avain-arvo-parien lisäämisen ja arvojen etsimisen avaimilla. Huomaa, että tämä koodi puuttuu törmäysten käsittelyn ja on tarkoitettu opetustarkoituksiin.

## Syvä sukellus
Assosiatiivisten taulukoiden konsepti on vanhempi kuin C, mutta kielen matalan tason luonne ei suoraan tue niitä sisäänrakennettuina tyyppinä. Tämä rohkaisee syvälliseen ymmärrykseen tietorakenteista ja algoritmeista, mukaan lukien hajautusmekanismien ymmärtäminen tehokkaaseen avain-arvo-kartoitukseen. Monet C-kirjastot ja kehykset tarjoavat kehittyneempiä lähestymistapoja assosiatiivisten taulukoiden toteuttamiseen, kuten GLibin `GHashTable`, joka tarjoaa kestävän toteutuksen täydellä törmäysten käsittelyllä, dynaamisella uudelleenkooltaamisella ja tuella mielivaltaisille avain- ja arvotyypeille.

Vaikka assosiatiivisten taulukoiden manuaalinen rakentaminen C:ssä voi tuntua työläältä verrattuna kieliin, joissa on sisäänrakennettu tuki, se tarjoaa arvokkaita näkemyksiä tietorakenteiden toiminnasta, teroittaen ohjelmoijan taitoja ongelmanratkaisussa ja optimoinnissa. Kuitenkin, tuotantokoodille tai monimutkaisemmille sovelluksille, olemassa olevien kirjastojen kuten GLibin käyttäminen on usein käytännöllisempi ja aikatehokkaampi lähestymistapa.
