---
title:                "Verifica se una directory esiste"
html_title:           "C++: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Verificare se una directory esiste è l'atto di controllare se una specifica cartella (o directory) esiste sulla macchina in uso. I programmatori lo fanno principalmente per evitare errori durante l'esecuzione del programma, ad esempio quando cercano di leggere o scrivere file in una directory che non esiste.

## Come fare:
Ecco un esempio di come verificare se una directory esiste in C++:

```C++
#include <filesystem>
#include <iostream>
  
int main()
{
    if(std::filesystem::exists("myDirectory")){
        std::cout << "Directory esiste.\n";
    } else {
        std::cout << "Directory non esiste.\n";
    }
}
```

L'output del programma sarà una delle due opzioni a seconda dell'esistenza della directory 'myDirectory':

```
Directory esiste.
```

oppure

```
Directory non esiste.
```

## Approfondimento
Il controllo dell'esistenza di una directory è una funzionalità che esiste da quasi sempre nel mondo del software. Precedentemente, in C++, si utilizzavano funzioni come `opendir()` e `stat()` per ottenere queste informazioni. 

Un metodo alternativo per controllare l'esistenza di una directory in C++ è l'uso di Boost Filesystem Library. 

La funzione `std::filesystem::exists()` è implementata in modo tale che ritorna `true` se il file o la directory specificata esiste; altrimenti ritorna `false`. Utilizza le funzioni di basso livello del sistema operativo per ottenere queste informazioni.

## Guarda anche
1. Documentazione su [std::filesystem](https://en.cppreference.com/w/cpp/filesystem) 
2. Documentazione su [Boost Filesystem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm) 
3. Guida su come fare la [gestione di file e directory in C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)