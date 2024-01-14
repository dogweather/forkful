---
title:                "C++: Verifica dell'esistenza di una cartella"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Molte volte quando si sviluppano dei programmi in C++, si ha la necessità di verificare se una directory esiste o meno. Questo può essere utile per gestire correttamente il flusso del programma e per evitare errori.

## Come fare

In C++ esistono diverse opzioni per verificare l'esistenza di una directory. La più semplice è utilizzare la funzione `std::experimental::filesystem::exists()`, che ritorna un valore booleano `true` se la directory esiste, altrimenti ritorna `false`.

```C++
#include <experimental/filesystem>
#include <iostream>

int main() {
    if (std::experimental::filesystem::exists("/percorso/alla/directory")) {
        std::cout << "La directory esiste!" << std::endl;
    } else {
        std::cout << "La directory non esiste!" << std::endl;
    }
    return 0;
}
```

L'output di questo codice sarà "La directory non esiste!", poiché il percorso indicato non esiste. 
Altre opzioni sono l'utilizzo delle funzioni `opendir()` o `stat()`, che permettono di controllare un file/directory specifico anziché un intero percorso. 

## Approfondimento

Esiste un modo per verificare l'esistenza di una directory in modo più preciso e sicuro. Prima di utilizzare la funzione `std::experimental::filesystem::exists()`, si può controllare anche il permesso di lettura della directory utilizzando la funzione `std::experimental::filesystem::is_readable()`. Questo garantisce che non solo la directory esiste, ma anche che è possibile accedervi senza problemi. Inoltre, si può verificare anche il tipo di oggetto (file o directory) utilizzando `std::experimental::filesystem::is_directory()` o `std::experimental::filesystem::is_regular_file()`. Questi accorgimenti possono essere utili per gestire al meglio le eccezioni all'interno del programma.

## Vedi anche

- [Documentazione C++ su std::experimental::filesystem](https://en.cppreference.com/w/cpp/filesystem)
- [Esempi di utilizzo delle funzioni is_readable e exists](https://www.tutorialspoint.com/checking-if-a-directory-exists-in-cplusplus)
- [Guida al controllo delle eccezioni in C++](https://www.geeksforgeeks.org/exception-handling-c/)