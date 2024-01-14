---
title:                "C: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

In questo post parleremo di come leggere gli argomenti della riga di comando in C. Se sei un programmatore alle prime armi o vuoi espandere le tue conoscenze linguistiche, questo argomento è perfetto per te!

## Come fare

Per leggere gli argomenti della riga di comando, dobbiamo prima definire la funzione "main" del nostro programma. All'interno di questa funzione, possiamo utilizzare due parametri: "argc" e "argv". "argc" è un intero che rappresenta il numero di argomenti passati da linea di comando, mentre "argv" è un array di stringhe che contiene gli argomenti stessi.

Vediamo un esempio pratico:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    // Stampa il numero di argomenti dalla riga di comando
    printf("Numero di argomenti: %d\n", argc);

    // Utilizza un ciclo for per stampare tutti gli argomenti
    for(int i = 0; i < argc; i++) {
        printf("Argomento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Se il nostro programma viene compilato e chiamato con gli argomenti "ciao" e "mondo", l'output sarà il seguente:

```
Numero di argomenti: 3
Argomento 0: ./a.out
Argomento 1: ciao
Argomento 2: mondo
```

In questo esempio, "a.out" rappresenta il nome del nostro programma compilato.

## Approfondimento

Ora che sappiamo come leggere gli argomenti della riga di comando, possiamo utilizzarli per renderlo più dinamico. Ad esempio, possiamo creare un programma che calcola la somma di due numeri inseriti dall'utente tramite riga di comando.

```C
#include <stdio.h>
#include <stdlib.h> // Incluso per utilizzare la funzione "atoi" per convertire la stringa in intero

int main(int argc, char *argv[]) {
    // Inizializza due variabili per contenere i numeri
    int num1, num2;
    
    // Verifica che siano stati inseriti due argomenti
    if(argc < 3) {
        printf("Inserisci due numeri come argomenti\n");
        return 1; // Restituisce 1 per indicare che c'è un errore
    }

    // Converte le stringhe in interi e le assegna alle variabili
    num1 = atoi(argv[1]);
    num2 = atoi(argv[2]);

    // Esegue l'operazione e stampa il risultato
    printf("La somma di %d e %d è: %d\n", num1, num2, num1 + num2);

    return 0;
}
```

Se il nostro programma viene chiamato con gli argomenti "2" e "5", l'output sarà:

```
La somma di 2 e 5 è: 7
```

Questo è solo uno dei tanti esempi di come possiamo utilizzare gli argomenti della riga di comando per rendere i nostri programmi più dinamici e personalizzabili.

## Vedi anche

- [Il sito ufficiale di programmazione in C](https://www.learn-c.org/)
- [Un tutorial sui parametri della riga di comando in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Un esempio avanzato di lettura degli argomenti dalla riga di comando](https://www.cprogramming.com/tutorial/c/lesson14.html)