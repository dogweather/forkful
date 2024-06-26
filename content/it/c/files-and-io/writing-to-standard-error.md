---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:56.223281-07:00
description: "Come fare: In C, il flusso `stderr` \xE8 utilizzato per scrivere messaggi\
  \ di errore. A differenza della scrittura sull'output standard con `printf`, la\u2026"
lastmod: '2024-03-13T22:44:44.018674-06:00'
model: gpt-4-0125-preview
summary: "In C, il flusso `stderr` \xE8 utilizzato per scrivere messaggi di errore."
title: Scrittura su errore standard
weight: 25
---

## Come fare:
In C, il flusso `stderr` è utilizzato per scrivere messaggi di errore. A differenza della scrittura sull'output standard con `printf`, la scrittura su `stderr` può essere eseguita utilizzando `fprintf` o `fputs`. Ecco come si può fare:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Questo è un messaggio di errore.\n");

    fputs("Questo è un altro messaggio di errore.\n", stderr);
    
    return 0;
}
```

Output di esempio (su stderr):
```
Questo è un messaggio di errore.
Questo è un altro messaggio di errore.
```

È importante notare che, sebbene l'output appaia simile a `stdout` nella console, quando si utilizza il reindirizzamento nel terminale, la distinzione diventa chiara:

```sh
$ ./il_tuo_programma > output.txt
```

Questo comando reindirizza solo l'output standard su `output.txt`, mentre i messaggi di errore appariranno ancora sullo schermo.

## Approfondimento
La distinzione tra `stdout` e `stderr` nei sistemi basati su Unix risale ai primi giorni di C e Unix. Questa separazione consente una gestione degli errori e un logging più robusti, poiché consente ai programmatori di reindirizzare i messaggi di errore indipendentemente dall'output standard del programma. Mentre `stderr` è non bufferizzato per impostazione predefinita per garantire l'output immediato dei messaggi di errore, il che aiuta nel debugging di crash e altre questioni critiche, `stdout` è tipicamente bufferizzato, il che significa che il suo output potrebbe essere ritardato fino a quando il buffer non viene svuotato (ad esempio, al termine del programma o allo svuotamento manuale).

Nelle applicazioni moderne, la scrittura su `stderr` è ancora rilevante, soprattutto per gli strumenti da riga di comando e le applicazioni server dove è cruciale distinguere tra messaggi di log regolari ed errori. Tuttavia, per una gestione degli errori più complessa, specialmente in applicazioni GUI o dove sono necessari meccanismi di logging più sofisticati, i programmatori potrebbero utilizzare librerie di logging dedicate che forniscono un maggiore controllo sulla formattazione dei messaggi, le destinazioni (ad esempio, file, rete) e i livelli di gravità (info, avviso, errore, ecc.).

Sebbene `stderr` fornisca un meccanismo fondamentale per la segnalazione degli errori in C, l'evoluzione delle pratiche di programmazione e la disponibilità di framework di logging avanzati significano che spesso è solo il punto di partenza per le strategie moderne di gestione degli errori.
