---
title:                "Generare numeri casuali"
html_title:           "C++: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
 Generare numeri casuali è un processo fondamentale nella programmazione che consente ai programmatori di creare una casistica più variata e realistica per i loro programmi. I numeri casuali possono essere utilizzati per la creazione di giochi, la simulazione di eventi casuali e molti altri scopi.

## Come Fare:
Ecco un codice di esempio in C++ per generare un numero casuale compreso tra 1 e 10:

```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main() {
    // inizializza il generatore di numeri casuali
    srand(time(0));
    
    // genera un numero casuale tra 1 e 10
    int numero = rand() % 10 + 1;
    
    // stampa il numero generato
    std::cout << "Numero casuale: " << numero << std::endl;
    
    return 0;
}
```

Output:

```
Numero casuale: 7
```

## Approfondimento:
La generazione di numeri casuali è una tecnica che risale ai primi giorni della programmazione, con algoritmi come il "linear congruential generator" sviluppato da Lehmer nel 1949. Oggi ci sono diverse alternative all'uso della funzione di generazione di numeri casuali inclusa in C++, come ad esempio la libreria [Boost.Random](https://www.boost.org/doc/libs/1_76_0/doc/html/boost_random.html).

## Vedi Anche:
- [Documentazione su rand() in C++](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Articolo su generazione di numeri casuali in C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)