---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Creare Numeri Casuali in C++
## Cosa e Perché?
Generare numeri casuali in C++ consiste in creare una sequenza di numeri che non presentano alcuna apparente correlazione. I programmatori lo fanno per simulare eventi che si verificano a caso nella vita reale, quali giochi di sorte, simulazione di scenari nel testing, o creazione di dati casuali per la crittografia.

## Come fare:
Ecco il codice per generare numeri casuali in C++.

 ```C++
#include <iostream>
#include <cstdlib>
#include <ctime>

int main () {
   //inizializza il generatore di numeri casuali
   srand(static_cast<unsigned int>(time(0)));
   
   //genera un numero casuale tra 1 e 6
   int randomNumber = rand() % 6 + 1;
   
   std::cout << "Numero casuale: " << randomNumber << std::endl;
   
   return 0;
}
  ```

Ecco un esempio di output:
`Numero casuale: 4`

## Approfondimento
Storicamente, in C++ si utilizzava la funzione `rand()` e `srand()` della libreria `cstdlib` per generare numeri casuali. Tuttavia, a partire da C++11, si consiglia di utilizzare il modulo `<random>` che fornisce funzionalità per generare numeri pseudo-casuali di alta qualità e con più opzioni.

Esistono alternative alla funzione `rand()`, come `random_device`, `default_random_engine`, e `uniform_int_distribution`, che possono essere utili per generare numeri casuali più sofisticati.

```C++
#include <iostream>
#include <random>
 
int main()
{
    std::random_device rd;   //utilizzato per ottenere un seme per il generatore di numeri
    std::mt19937 gen(rd());  //generatore di numeri Mersenne_twister
    std::uniform_int_distribution<> dist(1, 6); //distribuzione uniforme
    
    std::cout << "Numero casuale: " << dist(gen) << '\n';
}
```

## Vedere anche
Per ulteriori informazioni, consultate i seguenti collegamenti:

1. [`rand()` e `srand()`](http://www.cplusplus.com/reference/cstdlib/rand/)
2. [Modulo `<random>`](http://www.cplusplus.com/reference/random/)
3. [Generazione di numeri casuali in C++11](https://stackoverflow.com/questions/19665818/generate-random-numbers-using-c11-random-library)
4. [`random_device`, `default_random_engine`, e `uniform_int_distribution`](http://www.cplusplus.com/reference/random/)