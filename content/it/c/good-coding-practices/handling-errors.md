---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:02.536018-07:00
description: "Come fare: C non ha un supporto integrato per le eccezioni come alcuni\
  \ altri linguaggi. Invece, si affida a poche strategie convenzionali di gestione\u2026"
lastmod: '2024-03-13T22:44:44.007088-06:00'
model: gpt-4-0125-preview
summary: C non ha un supporto integrato per le eccezioni come alcuni altri linguaggi.
title: Gestione degli errori
weight: 16
---

## Come fare:
C non ha un supporto integrato per le eccezioni come alcuni altri linguaggi. Invece, si affida a poche strategie convenzionali di gestione degli errori, come il ritorno di valori speciali dalle funzioni e l'impostazione di variabili globali come `errno`.

**Ritornare Valori Speciali**

Le funzioni possono indicare errori restituendo un valore specifico che è improbabile sia un risultato valido. Ecco un esempio con gli interi:

```c
#include <stdio.h>

int inverso(int numero, double *risultato) {
    if (numero == 0) {
        return -1; // Caso di errore
    } else {
        *risultato = 1.0 / numero;
        return 0; // Successo
    }
}

int main() {
    double risultato;
    if (inverso(0, &risultato) < 0) {
        printf("Errore: Divisione per zero.\n");
    } else {
        printf("L'inverso è: %f\n", risultato);
    }
    
    return 0;
}
```

**Output:**
```
Errore: Divisione per zero.
```

**Verificare `errno`**

Per le funzioni di libreria, specialmente quelle che interagiscono con il sistema o con l'OS (come l'I/O su file), `errno` viene impostato quando si verifica un errore. Per utilizzarlo, includere `errno.h` e controllare `errno` dopo un sospetto fallimento:

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("inesistente.txt", "r");
    if (file == NULL) {
        printf("Errore nell'apertura del file: %s\n", strerror(errno));
    }
    
    return 0;
}
```

**Output:**
```
Errore nell'apertura del file: No such file or directory
```

## Approfondimento
Storicamente, il design minimalista del linguaggio di programmazione C ha escluso un meccanismo integrato di gestione delle eccezioni, riflettendo le sue origini di programmazione di sistema a basso livello, dove sono cruciali le massime prestazioni e il controllo diretto sul sistema. Invece, C adotta un approccio più manuale alla gestione degli errori che si adatta alla sua filosofia di dare ai programmatori il massimo controllo possibile, anche a costo della comodità.

Sebbene questo approccio si allinei bene agli obiettivi di progettazione di C, può anche portare a codice di controllo degli errori verboso e alla potenziale mancata verifica degli errori, che i linguaggi moderni affrontano con meccanismi strutturati di gestione delle eccezioni. Ad esempio, le eccezioni in linguaggi come Java o C# consentono un elaborazione centralizzata degli errori, rendendo il codice più pulito e la gestione degli errori più semplice. Tuttavia, le eccezioni introducono il loro sovraccarico e complessità, che potrebbero non essere ideali per la programmazione a livello di sistema, dove C eccelle.

Nonostante la sua grezzezza, questa gestione manuale degli errori in C ha informato il design della gestione degli errori in molti altri linguaggi, offrendo un modello in cui l'esplicità delle condizioni di errore può portare a un codice più prevedibile e debuggabile. Per i sistemi critici, dove i fallimenti devono essere gestiti con grazia, il paradigma di gestione degli errori di C—combinato con le migliori pratiche moderne come le librerie di gestione degli errori e le convenzioni—assicura robustezza e affidabilità.
