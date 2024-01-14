---
title:    "C: Lettura degli argomenti della riga di comando"
keywords: ["C"]
---

{{< edit_this_page >}}

## Perché

Ci sono molti motivi per cui potresti voler leggere gli argomenti della riga di comando in un programma C. Ad esempio, potresti voler fornire opzioni o parametri personalizzati all'avvio del tuo programma, o potresti voler gestire diversi input da parte dell'utente.

## Come Fare

La lettura degli argomenti della riga di comando in C è molto semplice. Innanzitutto, è necessario includere la libreria standard `stdio.h`, che offre le funzioni necessarie per interagire con la riga di comando. Quindi, si può utilizzare la funzione `main()` per accettare due parametri: `argc` (numero di argomenti) e `argv` (matrice di stringhe contenente gli argomenti). Vediamo un esempio di codice:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    // stampa il numero totale di argomenti passati
    printf("Numero di argomenti: %d\n", argc);

    // stampa tutti gli argomenti forniti
    for (int i = 0; i < argc; i++) {
        printf("Argomento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Se eseguiamo questo programma con il comando `./programma arg1 arg2`, otterremo una stampa simile a questa:

```
Numero di argomenti: 3
Argomento 0: ./programma
Argomento 1: arg1
Argomento 2: arg2
```

In questo modo, possiamo accedere ai vari argomenti forniti dall'utente e utilizzarli nel nostro programma.

## Approfondimento

Oltre al semplice utilizzo dei parametri `argc` e `argv`, è possibile leggere gli argomenti della riga di comando in modo più specifico. Ad esempio, è possibile utilizzare la funzione `getopt()` per leggere singolarmente ogni opzione fornita dall'utente con il relativo argomento. Inoltre, è possibile utilizzare `getopt_long()` per gestire opzioni più complesse, come quelle con valori opzionali o richieste multiple.

Raccomandiamo anche di utilizzare le costanti `getopt()` come `optopt`, `optarg` e `optind` per gestire gli errori e tenere traccia della posizione attuale nella lettura degli argomenti.

## Vedi Anche

- [Documentazione di `getopt()`](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [Tutorial su come leggere argomenti da riga di comando in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)