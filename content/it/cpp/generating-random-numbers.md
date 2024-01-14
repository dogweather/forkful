---
title:                "C++: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un concetto fondamentale nella programmazione. Può essere utilizzato in molte applicazioni, come ad esempio giochi, simulazioni, criptografia e test di algoritmi.

## Come

Il linguaggio di programmazione C++ offre diverse opzioni per generare numeri casuali. Vediamo alcuni esempi di codice utilizzando la libreria standard `random`:

```C++
// includiamo la libreria random
#include <random>
// per utilizzare la funzione di generazione di numeri casuali
using namespace std;

int main() {
   // definiamo un generatore di numeri casuali
   random_device rd;
   // definiamo un motore di generazione con distribuzione uniforme [0, 10)
   mt19937 gen(rd());
   uniform_int_distribution<> dist(0, 10);

   // generiamo 5 numeri casuali e li stampiamo a schermo
   for (int i = 0; i < 5; ++i) {
      cout << dist(gen) << ' ';
   }
   cout << endl;

   return 0;
}
```

L'output potrebbe essere ad esempio: `3 6 7 9 0`.

## Deep Dive

La libreria `random` di C++ offre diverse distribuzioni, come uniforme, normale, esponenziale e molti altre. Inoltre, è possibile impostare un seed per il generatore di numeri casuali, in modo da ottenere sempre gli stessi numeri casuali in un determinato punto del programma.

Un'altra opzione è quella di utilizzare la libreria di classe `rand` per generare numeri casuali. Questa libreria è già inclusa in C++ e offre funzioni come `srand` per impostare il seed e `rand` per generare un numero intero casuale.

## Vedi anche

- [Tutorial su generare numeri casuali in C++](https://www.learncpp.com/cpp-tutorial/random-number-generation/)
- [Documentazione ufficiale su libreria standard `random`](https://en.cppreference.com/w/cpp/numeric/random)
- [Generare numeri casuali con la libreria `rand`](https://www.geeksforgeeks.org/generating-random-number-range-c/)

Grazie per aver letto questo articolo sulle generazione di numeri casuali in C++. Speriamo che ti sia stato utile e che possa aiutarti nella tua prossima programmazione. Buona fortuna!