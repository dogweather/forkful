---
title:    "C++: Lettura degli argomenti della riga di comando"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori sono abituati a inserire dati di input in un programma attraverso un'interfaccia grafica. Tuttavia, ci sono molte ragioni per cui si potrebbe voler utilizzare anche gli argomenti della riga di comando come metodo di input. Ad esempio, si potrebbe voler scrivere uno script che può essere eseguito da una cronologia senza la necessità di un'interazione diretta dell'utente, o si potrebbe voler eseguire più istanze dello stesso programma con parametri diversi. In questo post, impareremo come leggere gli argomenti della riga di comando in un programma C++.

## Come fare

Per leggere gli argomenti della riga di comando in C++, è necessario utilizzare una funzione chiamata `main()`. Questo è il punto di partenza di ogni programma C++ e riceve due parametri: `argc` e `argv`. `argc` contiene il numero di argomenti passati alla riga di comando, mentre `argv` è un array di puntatori a stringhe, dove ogni elemento contiene un argomento. Ecco un esempio di come utilizzare questi parametri per stampare tutti gli argomenti passati:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << argv[i] << std::endl;
    }
    return 0;
}
```

Output:

```
./programma arg1 arg2 arg3
arg1
arg2
arg3
```

## Approfondimento

Esistono diverse funzioni utili per manipolare gli argomenti della riga di comando, come `std::stoi()` per convertire una stringa in un numero intero, `std::stof()` per convertire una stringa in un numero in virgola mobile e `std::istringstream` per convertire una stringa in un altro tipo di dato. Inoltre, è possibile utilizzare librerie come `getopt` per gestire in modo più avanzato gli argomenti della riga di comando e l'opzione `-h` per visualizzare una guida agli argomenti disponibile.

## Vedi anche

- [Documentazione ufficiale di C++](https://devdocs.io/cpp/)
- [Un esempio di utilizzo di `getopt` in C++](https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html)
- [Funzioni di stringhe utili in C++](https://www.cplusplus.com/reference/string/string/)