---
title:                "Lettura degli argomenti della riga di comando"
date:                  2024-01-20T17:55:20.214032-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando permette ai programmi di accettare input all'avvio. I programmatori lo fanno per personalizzare l'esecuzione o per specificare file e opzioni senza interazione con l'utente.

## How to:
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Hai fornito %d argomenti:\n", argc - 1);
    for (int i = 1; i < argc; ++i) {
        printf("%d: %s\n", i, argv[i]);
    }
    return 0;
}
```
Esegui: `./programma uno due tre`
Output:
```
Hai fornito 3 argomenti:
1: uno
2: due
3: tre
```

## Deep Dive
La convenzione di leggere gli argomenti dalla riga di comando esiste da quando il C è stato creato negli anni '70. Altre alternative includono l'uso di variabili d'ambiente o file di configurazione, ma nessuna è immediata come i parametri iniziali. `argc` indica quanti argomenti ci sono, mentre `argv` è un array di stringhe che li contiene. `argv[0]` è il nome del programma, gli argomenti iniziano quindi da `argv[1]`.

## See Also
- La documentazione di GCC su `argc` e `argv`: https://gcc.gnu.org/onlinedocs/
- Tutorial sul parsing avanzato con `getopt`: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- Concetti di base della riga di comando Unix: http://www.ee.surrey.ac.uk/Teaching/Unix/
