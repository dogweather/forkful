---
title:                "C: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una stringa in lettere minuscole può essere utile per uniformare l'input di un utente, rendere più semplice la ricerca all'interno di una stringa o semplicemente per motivi estetici.

## Come

Per convertire una stringa in lettere minuscole in linguaggio C, è necessario utilizzare la funzione "tolower" che prende come argomento un carattere e restituisce la sua controparte minuscola, se disponibile. Di seguito un esempio di codice:

```C
#include <stdio.h> 

int main() 
{ 
    char str[] = "CIAO A TUTTI"; 
    int i; 

    for (i = 0; str[i]!='\0'; i++) {
        str[i] = tolower(str[i]); // utilizzo della funzione tolower
    } 

    printf("La stringa convertita in minuscolo è: %s", str); // output: ciao a tutti

    return 0; 
} 
```

## Approfondimento

Quando si utilizza la funzione "tolower" per convertire una stringa in lettere minuscole, è importante tenere conto della codifica dei caratteri. Se si sta lavorando su una piattaforma che utilizza una codifica diversa rispetto alla codifica ASCII standard, potrebbero essere necessarie alcune modifiche al codice per ottenere il risultato desiderato.

Inoltre, è importante ricordare che la funzione "tolower" non supporta caratteri speciali o accenti, quindi se si vuole convertire una stringa contenente questi caratteri, è necessario implementare una logica aggiuntiva che gestisca queste eccezioni.

## Vedi anche

- [Funzione "tolower" in C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Codifica dei caratteri in C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Gestione dei caratteri speciali in C](https://www.programiz.com/c-programming/c-characters-strings)