---
title:                "Trova la lunghezza di una stringa."
html_title:           "C: Trova la lunghezza di una stringa."
simple_title:         "Trova la lunghezza di una stringa."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?: 

Trovare la lunghezza di una stringa è un'operazione comune tra i programmatori, che serve a determinare il numero di caratteri presenti all'interno di una stringa di testo. Questo può essere utile in molte situazioni, ad esempio per verificare che una stringa abbia una lunghezza specifica o per iterare attraverso i suoi caratteri.

## Come fare:

Ecco un esempio di codice in C che mostra come calcolare la lunghezza di una stringa utilizzando la funzione `strlen()` della libreria standard `<string.h>`:

```C
#include <stdio.h>
#include <string.h>

int main()
{
    // Definiamo la stringa da analizzare
    char str[] = "Ciao mondo!";

    // Chiamiamo la funzione strlen() e stampiamone il risultato
    printf("La lunghezza della stringa è: %d\n", strlen(str));

    return 0;
}

```

Output:
```
La lunghezza della stringa è: 11
```

## Approfondimento:

La funzione `strlen()` in realtà risale alla prima versione del linguaggio C, creato negli anni '70 da Dennis Ritchie. In alternativa, è possibile utilizzare anche la funzione `sizeof()`, che restituisce la dimensione in byte di un dato tipo di dato, per ottenere la lunghezza di una stringa.

Un dettaglio importante da considerare è che la lunghezza restituita dalla funzione `strlen()` non include il carattere terminatore di stringa `\0`. Inoltre, nell'analizzare stringhe multibyte è necessario utilizzare la funzione `mbstowcs()`.

## Vedi anche:

- [Documentazione ufficiale della funzione `strlen()`](https://www.cplusplus.com/reference/cstring/strlen/)
- [Esempi di utilizzo della funzione `strlen()`](https://www.geeksforgeeks.org/strlen-function-in-c-cpp/)
- [Altre funzioni utili per la manipolazione di stringhe in C](https://www.cprogramming.com/tutorial/c/lesson9.html)