---
title:                "Incrociando stringhe"
html_title:           "C: Incrociando stringhe"
simple_title:         "Incrociando stringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Perché

Concatenare le stringhe è un'operazione comune nella programmazione, utile per combinare più stringhe in una sola. Ciò può essere utile per creare output formattati, generare report o manipolare dati prima di inviarli ad altre funzioni.

## Come Fare

Per concatenare le stringhe in C, è possibile utilizzare la funzione `strcat()` presente nella libreria `string.h`. Ecco un esempio di codice che concatena due stringhe e ne stampa il risultato:

```C
#include <stdio.h>
#include <string.h>

int main() {

  char stringa1[] = "Ciao, ";
  char stringa2[] = "sono una stringa!";
  
  strcat(stringa1, stringa2);
  printf("%s", stringa1);
  
  return 0;
}

// Output: Ciao, sono una stringa!
```

È importante notare che la funzione `strcat()` unisce la seconda stringa alla prima, sovrascrivendo il terminatore di stringa `\0` e aggiungendone uno nuovo alla fine del risultato.

## Approfondimento

Esistono altre funzioni utili per manipolare e concatenare le stringhe in C, come ad esempio `strcpy()` per copiare una stringa in un'altra o `strncpy()` per copiare solo una determinata quantità di caratteri.

Inoltre, è importante prestare attenzione alla lunghezza delle stringhe che si stanno concatenando, per evitare di superare la dimensione massima della variabile o di causare un buffer overflow. Si consiglia di utilizzare la funzione `strncat()`, che permette di specificare il numero massimo di caratteri da concatenare.

## Vedi Anche

- [Documentazione ufficiale di `string.h`](https://www.cplusplus.com/reference/cstring)
- [Tutorial su come manipolare le stringhe in C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)