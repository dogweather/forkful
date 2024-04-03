---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:54.854464-07:00
description: "Hur man g\xF6r: C har inte inbyggt st\xF6d f\xF6r associativa arrayer\
  \ som vissa h\xF6gniv\xE5spr\xE5k, men du kan simulera dem med hj\xE4lp av strukturer\
  \ och hashtabeller.\u2026"
lastmod: '2024-03-13T22:44:38.374593-06:00'
model: gpt-4-0125-preview
summary: "C har inte inbyggt st\xF6d f\xF6r associativa arrayer som vissa h\xF6gniv\xE5\
  spr\xE5k, men du kan simulera dem med hj\xE4lp av strukturer och hashtabeller."
title: "Anv\xE4nda associativa arrayer"
weight: 15
---

## Hur man gör:
C har inte inbyggt stöd för associativa arrayer som vissa högnivåspråk, men du kan simulera dem med hjälp av strukturer och hashtabeller. Nedan följer ett förenklat exempel som använder en kombination av en struktur och en enkel hashfunktion för att implementera en associativ array för att lagra och komma åt heltal med strängnycklar.

Först, definiera en struktur för att representera ett enskilt nyckel-värde-par och en annan för att representera den associativa arrayen i sig:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} NyckelVardePar;

typedef struct {
    NyckelVardePar* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int value = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        value = value * 37 + key[i];
    }

    value = value % TABLE_SIZE;

    return value;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int value) {
    unsigned int slot = hash(key);

    NyckelVardePar* item = (NyckelVardePar*)malloc(sizeof(NyckelVardePar));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(AssocArray* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    AssocArray a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Utdata: 1
    printf("%d\n", find(&a, "key2")); // Utdata: 2

    return 0;
}
```

Detta exempel demonstrerar grundläggande operationer: initialisera en associativ array, infoga nyckel-värde-par och hitta värden med nycklar. Observera att den här koden saknar hantering av kollisioner och är menad för utbildningsändamål.

## Djupdykning
Konceptet med associativa arrayer föregår C, men språkets lågnivånatur stöder inte dem direkt som inbyggda typer. Detta uppmuntrar till en djupare förståelse för datastrukturer och algoritmer, inklusive hashningsmekanismer för effektiv nyckel-värde-mappning. Många C-bibliotek och ramverk erbjuder mer sofistikerade tillvägagångssätt för att implementera associativa arrayer, som GLib:s `GHashTable`, som ger en robust implementering komplett med hantering av kollisioner, dynamisk omstorlekning och stöd för godtyckliga nyckel- och värde typer.

Även om manuell konstruktion av associativa arrayer i C kan ses som besvärlig jämfört med språk med inbyggt stöd, erbjuder det ovärderliga insikter i datastrukturens inre arbete, vilket skärper en programmerares färdigheter i problemlösning och optimering. Dock, för produktionskod eller mer komplexa applikationer, är det ofta ett mer praktiskt och tidsbesparande tillvägagångssätt att utnyttja befintliga bibliotek som GLib.
