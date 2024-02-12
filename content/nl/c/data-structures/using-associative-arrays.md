---
title:                "Gebruik van associatieve arrays"
aliases:
- nl/c/using-associative-arrays.md
date:                  2024-02-03T18:10:44.093572-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van associatieve arrays"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Associatieve arrays, bekend in andere talen als mappen of woordenboeken, zijn sleutel-waarde paren die worden gebruikt voor efficiënte gegevensopzoeking en manipulatie. In tegenstelling tot traditionele arrays die integerindices gebruiken, gebruiken associatieve arrays sleutels, wat de gegevenstoegang intuïtiever en flexibeler maakt voor programmeurs.

## Hoe te:

C biedt geen ingebouwde ondersteuning voor associatieve arrays zoals sommige hogere programmeertalen, maar je kunt ze simuleren met behulp van structuren en hashing. Hieronder staat een vereenvoudigd voorbeeld met behulp van een combinatie van een struct en een eenvoudige hashfunctie om een associatieve array te implementeren voor het opslaan en toegang krijgen tot gehele getallen via tekenreeksleutels.

Definieer eerst een structuur om een enkel sleutel-waarde paar te vertegenwoordigen en een andere om de associatieve array zelf te vertegenwoordigen:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} KeyValuePair;

typedef struct {
    KeyValuePair* items[TABLE_SIZE];
} AssocArray;

unsigned int hash(char* key) {
    unsigned long int waarde = 0;
    unsigned int i = 0;
    unsigned int key_len = strlen(key);

    for (; i < key_len; ++i) {
        waarde = waarde * 37 + key[i];
    }

    waarde = waarde % TABLE_SIZE;

    return waarde;
}

void initArray(AssocArray* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(AssocArray* array, char* key, int waarde) {
    unsigned int slot = hash(key);

    KeyValuePair* item = (KeyValuePair*)malloc(sizeof(KeyValuePair));
    item->key = strdup(key);
    item->value = waarde;

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

    printf("%d\n", find(&a, "key1")); // Output: 1
    printf("%d\n", find(&a, "key2")); // Output: 2

    return 0;
}
```

Dit voorbeeld illustreert basisbewerkingen: het initialiseren van een associatieve array, het invoegen van sleutel-waarde paren en het vinden van waarden via sleutels. Merk op dat deze code geen afhandeling van botsingen heeft en bedoeld is voor educatieve doeleinden.

## Diepgaande verkenning

Het concept van associatieve arrays bestaat al langer dan C, maar de laag-niveau aard van de taal ondersteunt ze niet direct als ingebouwde typen. Dit moedigt een dieper begrip aan van gegevensstructuren en algoritmes, inclusief hashing-mechanismen voor efficiënte sleutel-waarde toewijzing. Veel C-bibliotheken en frameworks bieden geavanceerdere benaderingen om associatieve arrays te implementeren, zoals GLib's `GHashTable`, dat een robuuste implementatie biedt met afhandeling van botsingen, dynamisch aanpassen van de grootte en ondersteuning voor willekeurige sleutel- en waardetypes.

Hoewel het handmatig bouwen van associatieve arrays in C vergeleken kan worden met talen met ingebouwde ondersteuning als omslachtig, biedt het waardevolle inzichten in de werking van gegevensstructuren, waardoor de vaardigheden van een programmeur in probleemoplossing en optimalisatie worden gescherpt. Echter, voor productiecode of complexere toepassingen is het vaak een praktischere en tijdsefficiëntere aanpak om bestaande bibliotheken zoals GLib te gebruiken.
