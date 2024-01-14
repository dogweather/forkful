---
title:    "C++: Concatenazione di stringhe"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

La concatenazione di stringhe è un'operazione fondamentale nella programmazione in C++. Consiste nell'unire due o più stringhe per crearne una più lunga. Questa tecnica è molto utile per creare output dinamici, manipolare dati e creare stringhe complesse a partire da parti più semplici.

## Come Fare

Per concatenare due stringhe in C++, è possibile utilizzare l'operatore di addizione (+) o il metodo append(). Vediamo un esempio di come utilizzare entrambi:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Utilizzando l'operatore di addizione
    string saluto = "Ciao, ";
    string nome = "Paolo";
    string messaggio = saluto + nome;

    cout << messaggio << endl; // Stampa "Ciao, Paolo"

    // Utilizzando il metodo append()
    string parte1 = "Il numero è ";
    int numero = 10;
    string parte2 = " e il doppio è ";
    string risultato = parte1.append(to_string(numero)).append(parte2).append(to_string(numero * 2));

    cout << risultato << endl; // Stampa "Il numero è 10 e il doppio è 20"

    return 0;
}
```

In questo esempio, abbiamo dichiarato e inizializzato diverse stringhe, dopo di che le abbiamo concatenate utilizzando sia l'operatore di addizione che il metodo append(). Ricordati di utilizzare la libreria `<string>` per lavorare con le stringhe in C++.

## Approfondimento

La concatenazione di stringhe in C++ è possibile grazie all'overloading dell'operatore di addizione (+) per le stringhe e all'implementazione del metodo append() nella classe string. Inoltre, è importante notare che la concatenazione di stringhe è un'operazione inefficiente in termini di performance, soprattutto se utilizzata in un ciclo o su stringhe di grandi dimensioni. In questi casi, è consigliato utilizzare la classe `stringstream` per ottenere prestazioni migliori.

## Vedi Anche

- [Tutorial C++ su stringhe](https://www.programiz.com/cpp-programming/strings)
- [Documentazione C++ su stringhe](https://www.cplusplus.com/reference/string/string/)
- [Differenze tra l'utilizzo del operatore + e del metodo append() per le stringhe in C++](https://www.geeksforgeeks.org/difference-operator-vs-append-method-cpp-strings/)