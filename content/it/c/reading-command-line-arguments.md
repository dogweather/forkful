---
title:                "Lettura degli argomenti dalla riga di comando"
html_title:           "C: Lettura degli argomenti dalla riga di comando"
simple_title:         "Lettura degli argomenti dalla riga di comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Ciao lettori!

Se sei uno sviluppatore di C, devi essere familiare con il concetto di "command line arguments". Ma se sei nuovo alla programmazione, potresti chiederti: cos'è e perché i programmatori lo fanno?

## Cos'è e perché i programmatori lo fanno?

In parole semplici, leggere i command line arguments significa accettare gli input forniti dall'utente nella linea di comando quando si esegue un programma. Questi input possono essere stringhe, numeri, o anche opzioni per personalizzare il comportamento del programma. I programmatori lo fanno per rendere i loro programmi più interattivi e personalizzabili per l'utente.

## Come?

Per leggere i command line arguments in C, è necessario utilizzare la funzione ```main()``` e la variabile ```argc``` (che rappresenta il numero di argomenti) e ```argv``` (un array contenente gli argomenti). Ecco un esempio di codice e il relativo output:

```
#include <stdio.h>

int main(int argc, char *argv[])
{
    printf("Il programma è stato invocato con %d argomenti.\n", argc);

    for(int i = 0; i < argc; i++){
        printf("Argomento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Input:
```
./programma arg1 arg2 arg3
```

Output:
```
Il programma è stato invocato con 4 argomenti.
Argomento 0: ./programma
Argomento 1: arg1
Argomento 2: arg2
Argomento 3: arg3
```

## Scavando più a fondo

Ora che sai come leggere i command line arguments, forse ti chiedi perché questa funzionalità è stata incorporata nella programmazione C. All'inizio, il concetto di command line arguments è stato introdotto nei sistemi operativi per fornire un modo per passare opzioni e parametri ai programmi. Oggi, è ancora ampiamente utilizzato in molti contesti, come per esempio nei programmi di utilità e nei compilatori.

In alternativa, si può utilizzare la funzione ```getopt()``` della libreria C per leggere opzioni e argomenti da riga di comando in modo più avanzato.

## Vedi anche

Per ulteriori informazioni su come leggere i command line arguments in C, puoi consultare queste risorse:

- [Documentazione ufficiale della funzione main()](https://en.cppreference.com/w/c/language/main_function)
- [Documentazione ufficiale della funzione getopt()](https://en.cppreference.com/w/c/program/getopt)
- [Guida passo passo su come leggere command line arguments in C](https://www.linuxjournal.com/article/3808) 

E questo è tutto, spero che questo breve articolo ti sia stato utile. Buon codice!