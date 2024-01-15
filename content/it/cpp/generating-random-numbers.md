---
title:                "Generazione di numeri casuali"
html_title:           "C++: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché

Generare numeri casuali è un'operazione fondamentale nella programmazione, specialmente quando si lavora con algoritmi di apprendimento automatico o quando si vuole creare giochi o simulazioni realistiche.

# Come fare

Ecco alcuni esempi di codice in C++ per generare numeri casuali:

```
#include <iostream>
#include <cstdlib> //libreria per la gestione dei numeri casuali
using namespace std;

int main() {
    //genera un numero casuale tra 0 e 9
    int randomNumber = rand() % 10;
    cout << "Il numero casuale è: " << randomNumber << endl;

    //genera un numero casuale tra 1 e 100
    int randomNumber2 = rand() % 100 + 1;
    cout << "Un altro numero casuale: " << randomNumber2 << endl;

    //genera un numero casuale tra 5 e 20
    int min = 5;
    int max = 20;
    int randomNumber3 = rand() % (max - min + 1) + min;
    cout << "Un altro numero casuale: " << randomNumber3 << endl;

    return 0;
}
```

Esempio di output:

```
Il numero casuale è: 3
Un altro numero casuale: 57
Un altro numero casuale: 17
```

# Approfondimento

Per generare numeri casuali in modo più preciso e controllato, esistono diverse tecniche e algoritmi. Ad esempio, si può utilizzare la funzione `srand()` per impostare un valore di "seme" (seed) iniziale per la sequenza di numeri casuali. In questo modo, si otterranno sempre gli stessi numeri casuali ogni volta che si esegue il programma.

Inoltre, è importante comprendere che i numeri generati dalle funzioni `rand()` e `srand()` non sono realmente casuali, ma sono basati su una sequenza di calcoli matematici. È possibile utilizzare librerie specializzate per generare numeri casuali basati sulle probabilità, ad esempio la funzione `random_device` della libreria standard di C++, che utilizza algoritmi più avanzati e sicuri.

# Vedi anche

- [Documentazione di C++ su generazione di numeri casuali](https://en.cppreference.com/w/cpp/numeric/random)
- [Esempi di codice per la generazione di numeri casuali in C++](https://www.tutorialspoint.com/generate-random-numbers-in-cplusplus)