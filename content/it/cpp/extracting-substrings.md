---
title:    "C++: Estrarre sottostringhe"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
La ricerca e l'estrazione di sottostringhe (o substring) è una delle operazioni comuni nella programmazione. Questo processo è utile quando si desidera ottenere una parte specifica di una stringa più lunga. Ad esempio, si potrebbe voler estrarre il nome di una persona da una stringa che contiene il nome completo e altre informazioni. L'estrazione delle sottostringhe consente di scomporre e manipolare le stringhe in modo semplice ed efficiente.

## Come si fa
Per estrarre una sottostringa in C++, è possibile utilizzare la funzione `substr()` della libreria standard di C++. Questa funzione richiede due parametri: l'indice di inizio e la lunghezza della sottostringa. Ad esempio, supponiamo di avere la stringa "Buongiorno!" e vogliamo estrarre la sottostringa "Giorno". Possiamo fare così:

```
#include <iostream>
#include <string>
using namespace std;

int main() {
    string saluto = "Buongiorno!";
    string sottostringa = saluto.substr(4,5); // 4 è l'indice di inizio, 5 è la lunghezza della sottostringa
    cout << sottostringa; // output: "Giorno"
    return 0;
}
```

In questo esempio, abbiamo usato la funzione `substr()` per estrarre la sottostringa "Giorno" dalla stringa "Buongiorno!" a partire dall'indice 4 (lettera "g") per una lunghezza di 5 caratteri.

## Approfondimento
In C++, gli indici delle stringhe partono da 0. Questo significa che la prima lettera della stringa ha l'indice 0, la seconda ha l'indice 1 e così via. Inoltre, la funzione `substr()` includerà il carattere di inizio dell'indice ma escluderà il carattere finale. Ad esempio, nell'esempio precedente, l'indice di inizio è 4 ma la sottostinga include la lettera "g" (che ha l'indice 4) e finisce alla lettera "o" (che ha l'indice 8, ma escluso dalla sottostringa).

È possibile utilizzare questa funzionalità per estrarre sottostringhe di lunghezza variabile a partire da un indice arbitrario. Inoltre, è possibile utilizzare variabili o espressioni al posto dei parametri fissi per rendere dinamica l'estrazione delle sottostringhe.

## Vedi anche
- [Documentazione di C++ su `substr()`](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial su come utilizzare le stringhe in C++](https://www.programiz.com/cpp-programming/strings)
- [Esempi avanzati di estrazione delle sottostringhe in C++](https://www.geeksforgeeks.org/substring-substr-in-cpp/)