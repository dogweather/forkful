---
title:                "C: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Perché dovresti impegnarti nella generazione di numeri casuali? Molte volte è necessario generare numeri casuali all'interno di un programma per vari scopi, come simulazioni, giochi, crittografia e molto altro. Continua a leggere per scoprire come farlo utilizzando il linguaggio di programmazione C.

## Come fare
Prima di tutto, è necessario includere la libreria stdlib.h per poter utilizzare le funzioni per la generazione di numeri casuali.

```
#include <stdlib.h>
```
Una delle funzioni più utili è rand(), che genera un numero casuale tra 0 e RAND_MAX. Tieni presente che RAND_MAX è una costante definita nella libreria stdlib.h che varia a seconda della piattaforma. Per garantire una maggiore portabilità del codice, puoi utilizzare srand() per inizializzare il generatore di numeri casuali, fornendo un valore di "seme" univoco, solitamente l'orario di sistema in secondi.

```
srand(time(NULL)); //inizializza il generatore di numeri casuali

int numero = rand(); //genera un numero casuale tra 0 e RAND_MAX
```

Se vuoi generare un numero all'interno di un intervallo specifico, puoi utilizzare la seguente formula:

```
rand() % (max - min + 1) + min;
```

Ad esempio, se vogliamo generare un numero casuale tra 1 e 10, utilizzeremmo:

```
int numero = rand() % (10 - 1 + 1) + 1; //genera un numero casuale tra 1 e 10
```

## Approfondimento
La funzione rand() utilizza un algoritmo per generare numeri "pseudo-casuali", ovvero una sequenza di numeri che sembrano casuali ma in realtà sono determinati da una formula matematica. Questo significa che se si utilizza la stessa seme, si otterrà sempre la stessa sequenza di numeri casuali. Per questo motivo, è importante utilizzare una seme diversa durante ogni esecuzione del programma.

Se vuoi una maggiore casualità nella generazione di numeri, puoi utilizzare la funzione rand() insieme a una seme basata su variabili come l'orario di sistema, la posizione del mouse o l'input dell'utente.

## Vedi anche
- [Funzioni di generazione di numeri casuali in C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Utilizzo dei numeri casuali in C](https://www.tutorialspoint.com/cprogramming/c_random_numbers.htm)
- [Spiegazione dell'algoritmo di generazione casuale di C](https://en.wikipedia.org/wiki/Linear_congruential_generator) (in inglese)