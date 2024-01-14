---
title:    "C++: Generazione di numeri casuali"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è una pratica comune in molti tipi di programmazione, da giochi a simulazioni, passando per la crittografia e i test di stress sui sistemi. Utilizzare numeri casuali è un modo per rendere i programmi più imprevedibili e divertenti.

## Come fare
La generazione di numeri casuali in C++ è piuttosto semplice. Il modo più comune è utilizzare la funzione `rand()` della libreria standard `cstdlib`. Ecco un esempio di come generare un numero casuale compreso tra 1 e 10:

```C++
#include <cstdlib>
#include <iostream>
using namespace std;

int main() {

    // inizializzazione del generatore di numeri casuali
    srand(time(0));

    // generazione di un numero casuale
    int numero = rand() % 10 + 1;

    // output del numero generato
    cout << "Il numero casuale è: " << numero << endl;

    return 0;
}
```

Output:
```
Il numero casuale è: 7
```

## Approfondimento
Ci sono diverse cose da tenere in mente quando si generano numeri casuali in C++. Prima di tutto, è importante inizializzare il generatore di numeri casuali utilizzando la funzione `srand()` e passandogli un valore diverso ogni volta che si esegue il programma. In questo esempio, utilizziamo il tempo attuale come seed. Inoltre, è importante notare che `rand()` genererà sempre gli stessi numeri in sequenza se non si inizializza il generatore.

Inoltre, è possibile utilizzare funzioni matematiche per modificare i numeri generati da `rand()`, ad esempio utilizzando il modulo e l'operatore di addizione come nel nostro esempio per ottenere un numero compreso tra 1 e 10.

Infine, è possibile utilizzare funzioni come `srand()` e `rand()` in combinazione con altre funzioni per creare numeri casuali più specifici, come ad esempio numeri casuali con una distribuzione normale o una distribuzione esponenziale.

## Vedi anche
- [Funzione rand() della libreria standard cstdlib](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Esempi di generazione di numeri casuali in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Altre distribuzioni di numeri casuali in C++](https://www.iquilezles.org/www/articles/dice/dice.htm)