---
title:                "C: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché (Why)

Estrarre delle sottostringhe è un'operazione comune nella programmazione e può essere molto utile per molteplici motivi. Ad esempio, potresti dover manipolare dei dati di input per ottenere solo le informazioni che ti interessano, oppure dover effettuare delle operazioni su una parte specifica di una stringa. Imparare come estrarre delle sottostringhe può quindi semplificare il tuo codice e rendere il tuo programma più efficiente.

## Come (How To)

Per estrarre una sottostringa da una stringa in C, è possibile utilizzare la funzione `strncpy()`. Questa funzione prende come argomenti la stringa di destinazione, la stringa di origine e il numero di caratteri da copiare.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[50] = "Ciao a tutti!";
    char str2[50];

    // Estrarre i primi 4 caratteri di str1 e copiarli in str2
    strncpy(str2, str1, 4);

    printf("%s\n", str2); // Output: Ciao

    return 0;
}
```

La funzione `strncpy()` può avere delle limitazioni, in quanto se la stringa di origine è più corta della stringa di destinazione, potrebbero essere copiati dei caratteri indesiderati alla fine. In questi casi, è possibile utilizzare la funzione `strncat()` per concatenare la stringa di origine alla stringa di destinazione solo fino a quando non è stata copiata la giusta quantità di caratteri.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char str1[50] = "Ciao a tutti!";
    char str2[50];

    // Estrarre i primi 4 caratteri di str1 e concatenarli a str2
    strncat(str2, str1, 4);

    printf("%s\n", str2); // Output: Ciao

    return 0;
}
```

## Deep Dive

Per comprendere appieno il funzionamento della funzione `strncpy()`, è importante sapere che questa funzione aggiunge un carattere speciale di terminazione `'\0'` alla fine della stringa di destinazione, per indicare la fine della stringa. È quindi importante che la stringa di destinazione abbia una dimensione sufficientemente grande da poter contenere tutti i caratteri desiderati e il carattere di terminazione.

## Vedi Anche (See Also)

- [Funzioni di manipolazione delle stringhe in C (in italiano)](https://www.programmareinlinguaggio.com/le-funzioni-per-manipolare-le-stringhe-in-c/)
- [Documentazione ufficiale della funzione `strncpy()` (in inglese)](https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm)
- [Estrarre una stringa di caratteri da una stringa in C (in inglese)](https://www.geeksforgeeks.org/extract-a-substring-from-a-string-in-c/)