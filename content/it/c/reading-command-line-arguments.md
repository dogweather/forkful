---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cosa e Perche?

La lettura degli argomenti della riga di comando in C consiste nel fornire input al programma dalle opzioni specificate durante l'avvio. Lo si fa per rendere i programmi più versatili e interattivi personalizzando l'esecuzione senza dover modificare il codice.

## Come fare: 

Ecco l'essenziale. In C, gli argomenti della riga di comando vengono passati al programma attraverso i parametri `argc` e `argv` del metodo `main`.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int counter;
    printf("Program Name Is: %s", argv[0]);
    if(argc == 1)
        printf("\nNo Extra Command Line Argument Passed Other Than Program Name");
    if(argc >= 2)
    {
        printf("\nNumber Of Arguments Passed: %d",argc);
        printf("\n----Following Are The Command Line Arguments Passed----");
        for(counter = 1; counter < argc; counter++)
            printf("\nargv[%d] = %s\n", counter, argv[counter]);
    }
    return 0;
}
```

Se si esegue il programma come `./program arg1 arg2`, l'output sarà:

```
Program Name Is: ./program
Number Of Arguments Passed: 3
----Following Are The Command Line Arguments Passed----
argv[1] = arg1
argv[2] = arg2
```

## Approfondimenti:

Ho colpito il punto principale, ma ci sono cose che dovresti sapere. Gli argomenti della riga di comando sono antichi come il linguaggio C stesso, usati dai primi sistemi operativi UNIX. 

Le alternative moderne includono il parsing di file di configurazione o l'utilizzo di library come `getopt` per un parsing più complesso dei parametri. 

`argc` rappresenta il numero di argomenti passati e `argv` è un array di stringhe (puntatori a caratteri) che rappresentano i singoli argomenti forniti. `argv[0]` è il nome del programma stesso.

## Vedi anche:

Per un'analisi più dettagliata, vedi:
- [Command line arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Tutorial GNU - Argp](https://www.gnu.org/software/libc/manual/html_node/Argp.html)

Per le alternative, vedi:
- [C library function - getopt()](https://www.tutorialspoint.com/c_standard_library/c_function_getopt.htm)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)