---
title:                "Convertire una stringa in minuscolo"
html_title:           "C: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La conversione di una stringa in minuscolo è una pratica comune tra i programmatori che serve per modificare tutte le lettere di una stringa in caratteri minuscoli. Ciò può essere utile per confrontare le stringhe in modo esatto senza doversi preoccupare delle differenze tra lettere maiuscole e minuscole.

## Come fare:
Di seguito sono riportati due esempi di codice in C per convertire una stringa in minuscolo, entrambi utilizzando la libreria standard "string.h". Il primo metodo utilizza la funzione "tolower()", mentre il secondo utilizza una libreria di terze parti chiamata "strllower()". Ecco un output di esempio per entrambe le soluzioni:

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

int main()
{
    char stringa[] = "Ciao A tutti";
    int lunghezza = strlen(stringa);

    // Metodo 1: utilizzando tolower()
    for(int i = 0; i < lunghezza; i++){
        stringa[i] = tolower(stringa[i]);
    }

    printf("Metodo 1: %s\n", stringa); // Output: ciao a tutti

    // Metodo 2: utilizzando strlwr() dalla libreria "string.h"
    strlwr(stringa);
    printf("Metodo 2: %s\n", stringa); // Output: ciao a tutti

    return 0;
}
```

## Approfondimenti:
La conversione delle stringhe in minuscolo è diventata una pratica comune nei linguaggi di programmazione moderni, ma ha avuto origine nei primi giorni dei computer, quando i dati erano spesso memorizzati su nastri e le lettere maiuscole e minuscole erano rappresentate da codici diversi. Oggi ci sono anche altri metodi per ottenere lo stesso risultato, come l'utilizzo di espressioni regolari o la creazione di una funzione personalizzata.

Per gli utenti di MacOS, è possibile utilizzare la funzione "strlwr()" dalla libreria "strings.h"; mentre per gli utenti di Windows, esiste una funzione simile chiamata "strlwr_s()" dalla libreria "string.h".

## Vedi anche:
- Documentazione di tolower() su CppReference: https://en.cppreference.com/w/c/string/byte/tolower
- Spiegazione sull'utilizzo di strlwr() su GeeksforGeeks: https://www.geeksforgeeks.org/strlwr-function-in-c/
- Espressioni regolari in C: https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html