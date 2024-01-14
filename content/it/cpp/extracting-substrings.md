---
title:                "C++: Estrazione di sottostringhe"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
Estrarre sottostringhe è una pratica molto comune nella programmazione C++. Spesso, è necessario ottenere una parte specifica di una stringa più grande per elaborarla o manipolarla. In questo post, vedremo come estrarre efficientemente le sottostringhe in C++.

## Come fare
Per estrarre una sottostringa in C++, è possibile utilizzare la funzione `substr()` della libreria `<string>`. Questa funzione richiede due parametri: l'indice di inizio e il numero di caratteri da estrarre. Vediamo un esempio pratico di come utilizzarla:

```C++
#include<iostream>
#include<string>

int main() {
  std::string s = "Ciao a tutti";

  // Estrarre la sottostringa "Ciao"
  std::string sub = s.substr(0, 4);

  // Stampa la sottostringa
  std::cout << sub; // Output: Ciao

  return 0;
}
```

La funzione `substr()` è molto versatile e può essere utilizzata per estrarre sottostringhe in diversi modi. Ad esempio, è possibile specificare un solo parametro per estrarre una parte della stringa a partire dall'indice specificato fino alla fine della stringa.

```C++
// Estrarre la sottostringa "a tutti"
std::string sub = s.substr(5); // Il numero di caratteri da estrarre viene automaticamente calcolato

// Stampa la sottostringa
std::cout << sub; // Output: a tutti
```

Inoltre, è possibile utilizzare un valore negativo per l'indice di inizio per contare all'indietro a partire dalla fine della stringa.

```C++
// Estrarre la sottostringa "tutti"
std::string sub = s.substr(-5); // Inizia a contare dalla fine della stringa

// Stampa la sottostringa
std::cout << sub; // Output: tutti
```

## Approfondimento
Oltre alla funzione `substr()`, ci sono altre opzioni per estrarre sottostringhe in C++. Per esempio, si può utilizzare il puntatore alla stringa per accedere direttamente ai singoli caratteri e creare una nuova stringa con i caratteri desiderati. Tuttavia, questa è una soluzione più complicata e richiede una buona conoscenza dei puntatori.

Inoltre, è importante prestare attenzione alla gestione della memoria quando si lavora con sottostringhe. La funzione `substr()` crea una nuova stringa, quindi è necessario assicurarsi di dealloccare la memoria dopo l'utilizzo.

## Vedi anche
- Tutorial su come utilizzare la funzione `substr()` in C++: https://www.programiz.com/cpp-programming/library-function/string/substr
- Tutorial su come utilizzare i puntatori per estrarre sottostringhe in C++: https://www.tutorialspoint.com/cplusplus/cpp_strings.htm