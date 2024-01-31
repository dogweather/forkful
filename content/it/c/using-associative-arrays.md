---
title:                "Utilizzo di array associativi"
date:                  2024-01-30T19:10:03.795226-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di array associativi"
programming_language: "C"
category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cos'è & Perché?

Gli array associativi, o mappe hash, sono coppie chiave-valore che consentono di memorizzare e recuperare dati con una chiave. Sono incredibilmente utili in C poiché consentono un accesso ai dati più rapido rispetto alle liste, specialmente quando si gestiscono grandi quantità di dati.

## Come fare:

C non ha un supporto integrato per gli array associativi come alcuni altri linguaggi, ma possiamo usare strutture e alcune funzioni di libreria per ottenere funzionalità simili. Ecco un'implementazione semplice usando la libreria `uthash`, che dovrai includere nel tuo progetto.

Prima, definisci una struttura per contenere le tue coppie chiave-valore:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Questo sarà la nostra chiave
    char name[10]; // Questo è il valore associato alla nostra chiave
    UT_hash_handle hh; // Rende questa struttura hashabile
} persona;
```

Successivamente, aggiungiamo alcune voci e le recuperiamo:

```C
int main() {
    persona *le_mie_persone = NULL, *s;

    // Aggiungendo una voce
    s = (persona*)malloc(sizeof(persona));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(le_mie_persone, id, s);

    // Recuperando una voce
    int user_id = 1;
    HASH_FIND_INT(le_mie_persone, &user_id, s);
    if (s) {
        printf("Trovato: %s\n", s->name);
    }
    
    return 0;
}
```

L'output di esempio sarebbe:

```
Trovato: Alice
```

Non dimenticare di liberare la memoria allocata e deallocare la tabella hash al termine per evitare perdite di memoria.

## Approfondimento

Sebbene gli array associativi non siano nativi di C, librerie come `uthash` colmano abbastanza bene la lacuna, fornendo un modo piuttosto diretto per utilizzare questa funzionalità. Storicamente, i sviluppatori C dovevano implementare la loro versione di queste strutture dati, portando a implementazioni varie e spesso complesse, specialmente per coloro che sono solo all'inizio con il linguaggio.

Ricorda, l'efficienza dell'uso degli array associativi in C dipende molto da quanto bene la funzione hash distribuisce i valori nella tabella per minimizzare le collisioni. Mentre librerie come `uthash` offrono un buon equilibrio tra facilità d'uso e prestazioni, nelle applicazioni critiche dove le prestazioni sono fondamentali, potresti voler personalizzare o implementare la tua tabella hash.

Per applicazioni che richiedono massima efficienza, strutture dati alternative o addirittura altri linguaggi di programmazione con supporto integrato per gli array associativi potrebbero essere una scelta migliore. Tuttavia, per molte situazioni, specialmente quando si lavora già in un ambiente C, l'utilizzo di una libreria come `uthash` offre un equilibrio pratico tra prestazioni e comodità.
