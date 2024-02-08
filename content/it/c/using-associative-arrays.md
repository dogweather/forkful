---
title:                "Utilizzo di array associativi"
aliases:
- it/c/using-associative-arrays.md
date:                  2024-02-03T18:10:35.087343-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-associative-arrays.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi, noti in altri linguaggi come mappe o dizionari, sono coppie chiave-valore utilizzate per una ricerca e manipolazione efficienti dei dati. A differenza degli array tradizionali che utilizzano indici interi, gli array associativi utilizzano chiavi, rendendo l'accesso ai dati più intuitivo e flessibile per i programmatori.

## Come fare:

C non ha un supporto incorporato per gli array associativi come alcuni linguaggi di livello superiore, ma è possibile simularli utilizzando strutture e hashing. Di seguito è riportato un esempio semplicistico che utilizza una combinazione di una struct e una semplice funzione di hashing per implementare un array associativo per memorizzare e accedere a interi tramite chiavi stringa.

In primo luogo, definire una struttura per rappresentare una singola coppia chiave-valore e un'altra per rappresentare l'array associativo stesso:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TABLE_SIZE 128

typedef struct {
    char* key;
    int value;
} CoppiaChiaveValore;

typedef struct {
    CoppiaChiaveValore* items[TABLE_SIZE];
} ArrayAssoc;

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

void initArray(ArrayAssoc* array) {
    for (int i = 0; i < TABLE_SIZE; ++i) {
        array->items[i] = NULL;
    }
}

void insert(ArrayAssoc* array, char* key, int value) {
    unsigned int slot = hash(key);

    CoppiaChiaveValore* item = (CoppiaChiaveValore*)malloc(sizeof(CoppiaChiaveValore));
    item->key = strdup(key);
    item->value = value;

    array->items[slot] = item;
}

int find(ArrayAssoc* array, char* key) {
    unsigned int slot = hash(key);

    if (array->items[slot]) {
        return array->items[slot]->value;
    }
    return -1;
}

int main() {
    ArrayAssoc a;
    initArray(&a);

    insert(&a, "key1", 1);
    insert(&a, "key2", 2);

    printf("%d\n", find(&a, "key1")); // Output: 1
    printf("%d\n", find(&a, "key2")); // Output: 2

    return 0;
}
```

Questo esempio dimostra le operazioni di base: inizializzazione di un array associativo, inserimento di coppie chiave-valore e ricerca di valori per chiave. Si noti che questo codice manca della gestione delle collisioni ed è destinato a scopi educativi.

## Approfondimento

Il concetto di array associativi predata C, ma la natura di basso livello del linguaggio non li supporta direttamente come tipi incorporati. Questo incoraggia una comprensione più profonda delle strutture dati e degli algoritmi, inclusi i meccanismi di hashing per un mappaggio chiave-valore efficiente. Molte librerie e framework C offrono approcci più sofisticati per implementare gli array associativi, come il `GHashTable` di GLib, che fornisce un'implementazione robusta completa di gestione delle collisioni, ridimensionamento dinamico e supporto per tipi di chiavi e valori arbitrari.

Sebbene la costruzione manuale degli array associativi in C possa essere vista come ingombrante rispetto ai linguaggi con supporto incorporato, offre preziose intuizioni sul funzionamento interno delle strutture dati, affinando le competenze di un programmatore nella risoluzione di problemi e nell'ottimizzazione. Tuttavia, per un codice di produzione o applicazioni più complesse, sfruttare le librerie esistenti come GLib è spesso un approccio più pratico ed efficiente in termini di tempo.
