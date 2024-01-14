---
title:                "C++: Lettura degli argomenti della riga di comando"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

La lettura degli argomenti della riga di comando è un concetto fondamentale nella programmazione C++. La capacità di leggere e comprendere le informazioni fornite dall'utente tramite riga di comando è essenziale per lo sviluppo di programmi interattivi e dinamici. Inoltre, comprendere come utilizzare gli argomenti della riga di comando ti aiuterà a scrivere codice più efficiente e reattivo.

## Come fare

Per leggere gli argomenti della riga di comando in C++, è necessario utilizzare la funzione `main()` con due argomenti: l'`int` che rappresenta il numero di argomenti e un array di `char` che contiene gli argomenti stessi. Ad esempio:

```C++

int main(int argc, char *argv[]) {
    // codice per la lettura degli argomenti della riga di comando
}
```

Dentro la funzione `main()`, puoi utilizzare la variabile `argc` per sapere quante parole sono state passate come argomenti e l'array `argv` per accedere a ciascun argomento individualmente. Ad esempio, se un utente ha passato due parole come argomenti, si può accedere alla prima parola utilizzando `argv[1]` e alla seconda utilizzando `argv[2]`.

Ecco un semplice esempio che stampa gli argomenti della riga di comando:

```C++

#include <iostream>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        std::cout << "Argomento " << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

Output:

```
Argomento 0: nome_del_programma.exe
Argomento 1: Ciao
Argomento 2: Mondo
```

## Approfondimento

Oltre a leggere gli argomenti della riga di comando, è anche importante comprendere come gestire situazioni in cui gli utenti non forniscono abbastanza argomenti o forniscono argomenti in formato errato. Ad esempio, puoi utilizzare le funzioni `std::cin` o `std::getline` per ottenere dati da riga di comando e gestire eventuali errori utilizzando `if` e `else` statement.

Inoltre, puoi anche utilizzare librerie esterne come `boost::program_options` per semplificare la lettura e la gestione degli argomenti della riga di comando.

## Vedi anche

- [Documentazione ufficiale di C++ su argomenti della riga di comando](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutorial su lettura e scrittura di file in C++](https://dowhileblog.com/cpp/lettura-scrittura-file/)
- [Guida completa su C++ per principianti](https://www.sololearn.com/Course/CPlusPlus/)