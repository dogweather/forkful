---
title:                "C++: Generazione di numeri casuali"
programming_language: "C++"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una capacità fondamentale che ogni programmatore dovrebbe avere. Questo può essere utile per creare dati di prova o per simulare situazioni randomiche all'interno del software. Inoltre, può essere un'ottima abilità per programmi di gioco o altre applicazioni che richiedono numeri casuali.

## Come Fare

Ci sono diversi modi per generare numeri casuali in C++. Uno dei più comuni è utilizzare la funzione `rand()` dalla libreria `cstdlib`. Questa funzione genera un numero intero casuale tra 0 e `RAND_MAX`, una costante definita nella libreria.

Per utilizzare questa funzione, è necessario includere la libreria `cstdlib` nella parte superiore del file e inizializzare il generatore di numeri casuali utilizzando la funzione `srand()`, che richiede un valore intero come seed. Ad esempio, si può utilizzare il valore di tempo corrente come seed per ottenere una sequenza sempre diversa di numeri casuali.

Una volta inizializzato il generatore, è possibile utilizzare la funzione `rand()`, che restituirà un numero casuale ogni volta che viene chiamata. Per ottenere un numero all'interno di un range specifico, è possibile utilizzare l'espressione `min + rand() % (max - min + 1)`, dove `min` e `max` sono i limiti del range desiderato.

Un esempio di codice che genera 5 numeri casuali compresi tra 1 e 10 potrebbe essere il seguente:

```C++
#include <cstdlib>
#include <ctime>
#include <iostream>

int main() {
  // Inizializza il generatore di numeri casuali
  srand(time(0));
  
  // Genera e stampa i numeri casuali
  for (int i = 0; i < 5; i++) {
    int num = 1 + rand() % (10 - 1 + 1);
    std::cout << num << std::endl;
  }
  
  return 0;
}
```

L'output potrebbe essere ad esempio:

```
3
10
7
5
1
```

## Deep Dive

La funzione `rand()` non è l'unica opzione per generare numeri casuali in C++. È possibile utilizzare anche la libreria `<random>` che offre diversi generatori di numeri casuali avanzati, come `mt19937` o `minstd_rand`.

Inoltre, è importante ricordare che non tutti i generatori di numeri casuali sono effettivamente completamente casuali, ma si basano su algoritmi matematici. Quindi, se è necessario un alto livello di casualità, potrebbe essere utile utilizzare librerie di terze parti che offrono generatori basati su fenomeni fisici, come il rumore ambientale o l'attività delle radiazioni.

## Vedi Anche

- [Funzione `rand()` nella documentazione di C++](https://it.cppreference.com/w/cpp/numeric/random/rand)
- [Libreria `<random>` nella documentazione di C++](https://it.cppreference.com/w/cpp/numeric/random)
- [Libreria TrueRandom per generatori basati su fenomeni fisici](https://github.com/Bigjoe714/TrueRandom)