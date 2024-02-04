---
title:                "Generazione di numeri casuali"
date:                  2024-02-03T17:57:15.156817-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generazione di numeri casuali"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e perché?

Generare numeri casuali in C comporta la creazione di valori che sono imprevedibili e seguono una specifica distribuzione, come uniforme o normale. Questa capacità è cruciale per applicazioni che vanno dalle simulazioni e giochi alle operazioni crittografiche, dove l'imprevedibilità o la simulazione del caso reale è essenziale.

## Come fare:

In C, i numeri casuali possono essere generati utilizzando la funzione `rand()`, che fa parte della libreria standard C `<stdlib.h>`. Per impostazione predefinita, `rand()` produce numeri pseudo-casuali nell'intervallo da 0 a `RAND_MAX` (una costante definita in `<stdlib.h>`). Per avere più controllo sull'intervallo, i programmatori possono manipolare l'output di `rand()`.

Ecco un semplice esempio di generazione di un numero casuale tra 0 e 99:

```c
#include <stdio.h>
#include <stdlib.h> // Per rand() e srand()
#include <time.h>   // Per time()

int main() {
    // Inizializza il generatore di numeri casuali
    srand((unsigned) time(NULL));

    // Genera un numero casuale tra 0 e 99
    int numeroCasuale = rand() % 100;

    printf("Numero Casuale: %d\n", numeroCasuale);

    return 0;
}
```

L'output di esempio potrebbe variare ogni volta che esegui questo programma:

```
Numero Casuale: 42
```
Per generare numeri casuali in un intervallo diverso, puoi regolare il modulo operatore (`%`) di conseguenza. Per esempio, `rand() % 10` genera numeri da 0 a 9.

È importante notare che inizializzare il generatore di numeri pseudo-casuali (`srand()`) con l'ora corrente (`time(NULL)`) assicura sequenze diverse di numeri casuali ad ogni esecuzione del programma. Senza l'inizializzazione (`srand()`), `rand()` produrrebbe la stessa sequenza di numeri ogni volta che il programma viene eseguito.

## Approfondimento

La funzione `rand()` e la sua controparte per l'inizializzazione `srand()` fanno parte della libreria standard C da decenni. Sono basate su algoritmi che generano sequenze di numeri che sembrano essere casuali—da qui il termine "pseudo-casuali". L'algoritmo sottostante in `rand()` è tipicamente un generatore congruenziale lineare (LCG).

Sebbene `rand()` e `srand()` siano sufficienti per molte applicazioni, presentano limitazioni note, specialmente riguardo alla qualità della casualità e alla potenziale prevedibilità. Per applicazioni che richiedono casualità di alta qualità, come le operazioni crittografiche, si dovrebbero considerare alternative come `/dev/random` o `/dev/urandom` (su sistemi simili a Unix), o API fornite da librerie crittografiche.

Con l'introduzione di C11, lo standard ISO C ha incluso un nuovo header, `<stdatomic.h>`, offrendo un controllo più raffinato per le operazioni concorrenti, ma non riguardanti direttamente la casualità. Per una vera casualità in C, gli sviluppatori spesso si rivolgono a librerie specifiche della piattaforma o esterne che offrono algoritmi migliori o fanno uso di fonti di entropia hardware.

Ricorda, mentre `rand()` serve come un mezzo semplice e accessibile per generare numeri pseudo-casuali, i suoi usi nelle applicazioni moderne sono limitati dalla qualità e dalla prevedibilità del suo output. Quando sono richieste soluzioni più robuste, specialmente per applicazioni attente alla sicurezza, esplorare oltre la libreria standard è vivamente consigliato.
