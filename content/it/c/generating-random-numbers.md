---
title:    "C: Generazione di numeri casuali"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile in molte situazioni, come nella scelta di un vincitore per un concorso o nella creazione di giochi e simulazioni. È importante comprendere il concetto di generazione di numeri casuali per poter applicare correttamente questa tecnica nel tuo codice.

## Come fare

Ci sono diverse funzioni utilizzate in C per generare numeri casuali. Una di queste è la funzione rand() che restituisce un numero intero casuale nell'intervallo 0 e RAND_MAX. Per impostare un seed e ottenere numeri diversi ad ogni esecuzione, puoi utilizzare la funzione srand(). Di seguito puoi vedere un esempio di come utilizzare queste funzioni per generare dieci numeri casuali e stamparli a schermo:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    int i, randomNum;
    srand(time(0)); // impostiamo il seed utilizzando l'orario corrente
    for(i = 0; i < 10; i++)
    {
        randomNum = rand();
        printf("%d ", randomNum); // stampiamo il numero casuale generato
    }
    return 0;
}
```
**Output:**
```bash
1942500626 424238335 135497281 478931781 1334993101 419440389 57871710 1651009773 1189641421 367477592
```

Esistono anche altre funzioni utilizzabili per ottenere numeri casuali, come ad esempio la funzione rand_r() per generare numeri casuali in modo thread-safe.

## Approfondimento

La generazione di numeri casuali è un processo algoritmico che si basa su un valore di partenza (seed) e su un algoritmo di generazione. È importante che il valore di seed sia diverso ad ogni esecuzione per ottenere numeri casuali differenti. Per questo motivo, si utilizza spesso l'orario corrente come seed.

Inoltre, è importante notare che i numeri generati non sono effettivamente casuali, ma seguono un algoritmo specifico. Per questo motivo, è possibile utilizzare diverse tecniche di analisi per verificare la "casualità" dei numeri generati.

## Vedi anche
- [Documentazione ufficiale di C sulla funzione rand()](https://www.gnu.org/software/libc/manual/html_node/Pseudorandom-Numbers.html)
- [Tutorial su come generare numeri casuali in C](https://www.programiz.com/c-programming/examples/random-numbers)
- [Approfondimenti sulla generazione di numeri casuali in C](https://www.geeksforgeeks.org/generating-random-number-range-c/)