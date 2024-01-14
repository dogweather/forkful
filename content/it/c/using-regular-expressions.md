---
title:                "C: Utilizzare le espressioni regolari"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Perché utilizzare le espressioni regolari in programmazione

Le espressioni regolari sono un utile strumento di programmazione che permette di trovare e manipolare stringhe in modo efficiente. Con l'aiuto delle espressioni regolari, puoi cercare e sostituire testo in modo rapido e preciso. In questo post, imparerai come utilizzare le espressioni regolari in linguaggio C per semplificare la tua vita di programmazione.

# Come utilizzare le espressioni regolari in C

Per utilizzare le espressioni regolari in C, è necessario includere la libreria "regex.h" nel tuo programma. Una volta inclusa, puoi utilizzare le funzioni della libreria per ricerca e manipolazione di stringhe.

Di seguito è riportato un esempio di codice che cerca una parola specifica all'interno di una stringa utilizzando un'espressione regolare e stampa la posizione in cui è stata trovata:

```C
#include <stdio.h>
#include <regex.h>

int main() {

    // Definisci una stringa e un'espressione regolare
    char string[] = "Questo è un esempio di stringa";
    char pattern[] = "esempio";

    // Crea una struttura regex
    regex_t regex;

    // Compila l'espressione regolare
    int result = regcomp(&regex, pattern, 0);

    // Cerca la stringa usando l'espressione regolare
    regmatch_t matches[1];
    result = regexec(&regex, string, 1, matches, 0);

    // Stampa la posizione in cui è stata trovata la parola "esempio"
    if (result == 0){
        printf("La parola 'esempio' è stata trovata nella posizione %d", matches[0].rm_so);
    }

    // Libera la memoria della regex
    regfree(&regex);

    return 0;
}
```

Output:

```
La parola 'esempio' è stata trovata nella posizione 11
```

# Approfondimento sull'utilizzo delle espressioni regolari

Le espressioni regolari possono essere utilizzate in modo più avanzato per effettuare ricerche più complesse, come ricerca e sostituzione di pattern multipli e gestione di input utente dinamici. È possibile utilizzare diversi metacaratteri all'interno delle espressioni regolari per effettuare ricerche più specifiche.

Per ulteriori informazioni sull'utilizzo delle espressioni regolari in C, ti consigliamo di consultare la documentazione ufficiale della libreria "regex.h" e fare pratica con diversi esempi.

# Vedi anche

- [Documentazione ufficiale della libreria "regex.h"](https://pubs.opengroup.org/onlinepubs/9699919799/functions/regcomp.html)
- [Esempi di utilizzo delle espressioni regolari in C](https://www.thegeekstuff.com/2011/01/regular-expressions-in-c/)
- [Tutorial avanzato sulle espressioni regolari in C](https://www.cprogramming.com/tutorial/regular-expressions-c.html)