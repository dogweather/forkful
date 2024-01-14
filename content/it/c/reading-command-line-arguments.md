---
title:                "C: Leggere gli argomenti della linea di comando"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
 

La lettura degli argomenti della riga di comando è un'abilità fondamentale per ogni programmatore C. Ciò consente di creare programmi dinamici e interattivi che possono ricevere input da parte dell'utente durante l'esecuzione. In questo post, impareremo come leggere gli argomenti della riga di comando in C per migliorare le nostre capacità di programmazione.

## Come Fare

Per leggere gli argomenti della riga di comando in C, dobbiamo includere la libreria "stdio.h" e dichiarare due argomenti nel main: "argc" e "argv". "argc" è un intero che indica il numero di argomenti passati alla riga di comando, mentre "argv" è un array di stringhe che contiene i parametri passati.

Per esempio, supponiamo di avere un programma chiamato "hello.c" che prende un argomento dalla riga di comando e poi lo stampa a schermo. Il codice sarebbe il seguente:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  printf("Hello %s!", argv[1]);
  return 0;
}
```

Se compiliamo e eseguiamo il programma utilizzando "hello Maria" come argomento, verrà stampato a schermo "Hello Maria!".

## Approfondimento

Oltre all'uso dei parametri della riga di comando come input, possiamo anche utilizzarli per opzioni e argomenti opzionali. Leremo più in dettaglio sull'utilizzo degli argomenti opzionali e sulla manipolazione delle stringhe passate come argomenti.

Inoltre, è importante notare che gli argomenti della riga di comando sono di tipo stringa. Ciò significa che dobbiamo convertire i valori in altri tipi di dati se vogliamo utilizzarli in operazioni matematiche, per esempio.

## Vedi Anche

- [Dichiarazione e utilizzo di argomenti della riga di comando in C](https://www.programiz.com/c-programming/c-command-line-arguments)
- [Utilizzo delle funzioni di conversione di stringhe in C](https://www.tutorialspoint.com/c_standard_library/c_function_atoi.htm)
- [Introduzione alla programmazione in C](https://www.html.it/guide/guida-c/)