---
title:                "Trasformare una stringa in minuscolo"
html_title:           "C: Trasformare una stringa in minuscolo"
simple_title:         "Trasformare una stringa in minuscolo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler convertire una stringa in minuscolo in C. Ad esempio, potresti dover confrontare stringhe o filtrare i dati in base alla loro formattazione.

## Come fare
Per farlo, è necessario utilizzare la funzione `tolower()` inclusa nella libreria standard `ctype.h`. Ecco un esempio di come puoi utilizzarla:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char stringa[] = "Ciao Mondo!";
  
  // Converte tutti i caratteri della stringa in minuscolo
  for (int i = 0; stringa[i] != '\0'; i++) {
    stringa[i] = tolower(stringa[i]);
  }
  
  printf("%s", stringa);
  
  return 0;
}
```
Output: ciao mondo!

## Approfondimento
La funzione `tolower()` prende in input un carattere e restituisce la sua versione in minuscolo se è una lettera. Se il carattere in input non è una lettera, viene restituito inalterato. È importante notare che questa funzione lavora solo con i caratteri ASCII, quindi potrebbe non funzionare correttamente con caratteri di altre lingue.

## Vedi anche
- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Guida completa per principianti di C](https://www.learn-c.org/)
- [Utilizzo delle funzioni di libreria in C](https://www.tutorialspoint.com/c_standard_library/index.htm)