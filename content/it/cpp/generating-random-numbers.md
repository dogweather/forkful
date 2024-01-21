---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:50.807510-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Generare numeri casuali significa creare valori imprevedibili tramite un algoritmo. Programmare con numeri casuali è fondamentale per giochi, simulazioni, test, sicurezza informatica e analisi statistiche.

## How to: (Come fare:)
Ecco come generare numeri casuali in modo semplice con C++:

```C++
#include <iostream>
#include <random>

int main() {
    // Inizializziamo il motore con un seme basato sul tempo
    std::mt19937 mt{ std::random_device{}() };
    
    // Distribuzione uniforme tra 1 e 10
    std::uniform_int_distribution<int> dist(1, 10);

    // Generiamo e stampiamo cinque numeri casuali
    for(int i = 0; i < 5; ++i) {
        std::cout << dist(mt) << "\n";
    }

    return 0;
}
```

Esempio di output:
```
4
7
3
9
1
```

## Deep Dive (Approfondimento)
La generazione di numeri casuali in programmazione ha un lungo percorso, a cominciare dai semplici metodi come `rand()` del C, fino ai generatori moderni del C++11 come `std::mt19937`, molto più sofisticati e meno prevedibili. 

Il vecchio `rand()` dipendeva da un seme globale e produceva una sequenza di numeri che poteva essere ripetitiva e quindi meno casuale. Con C++11, si preferisce usare il `<random>` header, che offre una varietà di generatori e distribuzioni per diversi casi d'uso.

Il `std::mt19937` è un generatore basato sull'algoritmo Mersenne Twister, che è veloce e produce una sequenza di numeri estremamente lunga prima di ripetere. Le distribuzioni, come `std::uniform_int_distribution`, prendono numeri da questi generatori e li adattano a specifici intervalli o modelli di distribuzione.

## See Also (Vedi anche)
- [std::mt19937 reference](http://en.cppreference.com/w/cpp/numeric/random/mersenne_twister_engine)
- [std::uniform_int_distribution reference](http://en.cppreference.com/w/cpp/numeric/random/uniform_int_distribution)
- Tutorial C++ su random: [https://www.learncpp.com/cpp-tutorial/random-number-generation/](https://www.learncpp.com/cpp-tutorial/random-number-generation/)
- Approfondimenti sulla sicurezza dei numeri casuali: [https://channel9.msdn.com/Events/GoingNative/2013/rand-Considered-Harmful](https://channel9.msdn.com/Events/GoingNative/2013/rand-Considered-Harmful)