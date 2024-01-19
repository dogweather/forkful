---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Leggere gli argomenti da linea di comando in C++ permette all'utente di inserire parametri direttamente al momento dell'esecuzione del programma. Quest'operazione è utile per configurare l'ambiente di esecuzione, passare valori senza cambiare il codice o pilotare l'output.

## Come fare:
Vediamo un esempio di come leggere gli argomenti da linea di comando:
```C++
#include<iostream>
int main(int argc, char *argv[]) {
    for(int i = 0; i < argc; i++) {
        std::cout << "Argomento " << i << ": " << argv[i] << "\n";
    }
    return 0;
}
```
Nel caso in cui eseguiamo `./programma arg1 arg2`, l'output sarà:
```
Argomento 0: ./programma
Argomento 1: arg1
Argomento 2: arg2
```
## Approfondimenti:
La lettura di argomenti da linea di comando risale all'era dei sistemi operativi Unix, dove applicazioni come `grep` e `ls` basavano il loro comportamento su quest'input. C++ permette questo accesso diretto all'input da linea di comando attraverso i parametri `argc` e `argv` della funzione `main`. Un'alternativa è l'utilizzo di librerie esterne come `boost.program_options` che offre maggiore flessibilità e opzioni di parsing avanzate. Nondimeno, l'approccio nativo ha il vantaggio di non richiedere alcuna dipendenza aggiuntiva ed è sufficiente per molti scenari comuni.

## Vedi Anche:
1. ["Command line arguments in C/C++"](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
2. [Documentazione di Boost.Program_options](https://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html)
3. ["How do I handle command line arguments in C++?"](https://stackoverflow.com/questions/3024197/how-do-i-handle-command-line-arguments-in-c) su StackOverflow.