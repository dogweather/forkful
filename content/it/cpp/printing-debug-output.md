---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è & Perchè?  
Stampare output per il debug significa mostrare informazioni extra dal tuo programma per comprendere meglio ciò che sta accadendo all'interno. I programmatori lo fanno per identificare e risolvere i problemi nel codice.

## Come fare:
Ecco un semplice esempio di stampa del debug output in C++. Utilizziamo l'istruzione `cout`.

```C++
#include <iostream>
using namespace std;
int main() {
    int num = 5;
    cout << "Debug: Numero è " << num << endl;
    return 0;
}
```
Output:
```
Debug: Numero è 5
```

## Approfondimenti
(1) Storicamente, la stampa per il debug è stato uno dei primi metodi utilizzati per il debugging. Prima dell'arrivo degli IDE e dei debugger avanzati, questo era lo strumento di debugging predominante.

(2) Ci sono molte alternative per stampare debug output in C++. Ad esempio, potresti utilizzare le librerie dedicate come `log4cpp` o `spdlog`, che forniscono un'interfaccia di logging più flessibile e potente.

(3) La stampa del debug output in C++ è implementata tramite l'operatore di streaming `<<` che invia l'output a `cout`, che a sua volta visualizza il messaggio nella console.

## Altro
Per ulteriori informazioni controlla i seguenti link:

1. [Cout in C++ (en): GeeksForGeeks](https://www.geeksforgeeks.org/basic-input-output-c/)
2. [Debugging (en) - Wikipedia](https://en.wikipedia.org/wiki/Debugging)