---
title:                "Trova la lunghezza di una stringa"
html_title:           "C: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai imparando a programmare in C, una delle prime cose che vorrai imparare è come trovare la lunghezza di una stringa. Sapere come fare questa operazione ti aiuterà a gestire le stringhe all'interno del tuo codice e renderà il tuo programma più efficiente.

## Come fare

La lunghezza di una stringa in C è determinata dal numero di caratteri all'interno della stringa, incluso il carattere terminatore '\0'. Per trovare la lunghezza, possiamo utilizzare la funzione *strlen* inclusa nella libreria standard *string.h*.

```
#include <string.h>
#include <stdio.h>

int main() {
  // Dichiarazione della stringa
  char stringa[] = "Ciao mondo!";
  
  // Utilizzo della funzione strlen per trovare la lunghezza
  int lunghezza = strlen(stringa);
  
  // Stampa del risultato
  printf("La lunghezza della stringa è %d\n", lunghezza);
  
  return 0;
}
```

Output: La lunghezza della stringa è 11

## Approfondimento

La funzione *strlen* in realtà controlla ogni carattere della stringa fino a quando non trova il carattere terminatore '\0'. Questo significa che se la tua stringa non è terminata correttamente, la lunghezza trovata potrebbe essere errata.

Inoltre, sarebbe utile conoscere il tipo di dato restituito dalla funzione *strlen*. Essa restituisce un *size_t*, che è semplicemente un alias per un intero senza segno. Questo ci aiuta a gestire stringhe di grandi dimensioni in modo più efficiente.

## Vedi anche

- [Documentazione ufficiale della funzione strlen](https://www.cplusplus.com/reference/cstring/strlen/)
- [Come dichiarare e manipolare stringhe in C](https://it.wikipedia.org/wiki/Stringa_(informatica)#In_C)
- [Come risolvere eventuali problemi di memoria con le stringhe in C](https://www.geeksforgeeks.org/problems-with-input-string-in-c-set-1/)