---
title:                "Leggere gli argomenti dalla linea di comando"
html_title:           "C: Leggere gli argomenti dalla linea di comando"
simple_title:         "Leggere gli argomenti dalla linea di comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
In questo articolo, scoprirai come leggere gli argomenti della riga di comando utilizzando il linguaggio di programmazione C. Questa abilità ti permetterà di creare programmi che possono interagire con l'utente e accettare input personalizzati.

## Come Fare
Per leggere gli argomenti della riga di comando in C, puoi utilizzare la funzione `main()` con due parametri, `argc` e `argv`, che rappresentano rispettivamente il numero di argomenti passati e un array contenente gli argomenti stessi. Ad esempio:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Il numero di argomenti passati è: %d\n", argc);

    for (int i = 0; i < argc; i++) {
        printf("Argomento %d: %s\n", i, argv[i]);
    }

    return 0;
}
```

Se eseguiamo questo programma con il comando `./programma arg1 arg2 arg3`, l'output sarà il seguente:

```
Il numero di argomenti passati è: 4
Argomento 0: ./programma
Argomento 1: arg1
Argomento 2: arg2
Argomento 3: arg3
```

Puoi quindi utilizzare questi argomenti all'interno del codice per creare un programma dinamico e interattivo.

## Approfondimento
Oltre alla funzione `main()`, esistono altre modalità per leggere gli argomenti della riga di comando in C. Ad esempio, è possibile utilizzare la funzione `getopt()` per gestire opzioni e argomenti opzionali. È anche possibile creare una stringa con gli argomenti passati utilizzando la funzione `getenv()`.

## Vedi Anche
- [Documentazione ufficiale di C](https://gcc.gnu.org/onlinedocs/gcc/Command-Line-Arguments.html)
- [Tutorial su come leggere gli argomenti della riga di comando in C](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Esempi di utilizzo della funzione getopt()](https://www.geeksforgeeks.org/getopt-function-in-c-to-parse-command-line-arguments/)
- [Esempi di utilizzo della funzione getenv()](https://fresh2refresh.com/c-programming/c-system-programming/c-getenv-function/)