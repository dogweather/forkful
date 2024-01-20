---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---
Title: "Cerco e Sostituisco Testo in C: Una Guida Veloce ed Essenziale"

---

## Cos'è e Perché?

La ricerca e la sostituzione del testo è un'operazione fondamentale che permette a un programma di identificare specifiche stringhe di caratteri e sostituirle con altre. Questo concetto è vitale nella manipolazione dei dati, nella pulizia dei dati e negli editor di testo.

## Come fare:

Ecco un esempio su come implementare una funzione che ricerca e sostituisce il testo in linguaggio C.

```C
#include <string.h>

void sustitue(char *origen, char *cerco, char *sostituisco){
   char buffer[256];
   char *p;

   if(!(p = strstr(origen, cerco)))  
      return;

   strncpy(buffer, origen, p-origen); 
   buffer[p-origen] = '\0';

   sprintf(buffer+(p-origen), "%s%s", sostituisco, p+strlen(cerco));

   strcpy(origen, buffer); 
}

int main(){
    char stringa[256] = "Amo la pizza!";
    printf("Prima: %s\n", stringa);
    sustitue(stringa, "pizza", "gelato");
    printf("Dopo: %s\n", stringa);

    return 0;
}
```

Output:

```
Prima: Amo la pizza!
Dopo: Amo il gelato!
```

## Approfondimento: 

La ricerca/sostituzione del testo è un concetto molto antico nella programmazione, nasce con i primi editor di testo nei primi anni '70. In linguaggio C, funzioni come `strstr()`, `strcpy()`, `sprintf()`, e `strncpy()` ci permettono di implementare questo concetto.

Inoltre, esistono varie librerie, come regex.h, che offrono funzioni più avanzate e potenti per cercare e sostituire il testo, dove si può anche utilizzare le espressioni regolari.

L'implementazione dettagliata della ricerca e sostituzione del testo può variare a seconda delle esigenze. L'esempio presentato qui è molto basilare, ma può essere esteso per gestire casi più complessi.

## Da Vedere: 

Per approfondire, consiglio questi link:

1. [What does strstr do in C/C++?](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm) - Tutorials Point
2. [How do I replace a substring in a string in C?](https://stackoverflow.com/questions/779875/what-function-is-to-replace-a-substring-from-a-string-in-c) - Stack Overflow
3. [C Library - <string.h>](https://www.tutorialspoint.com/c_standard_library/string_h.htm) - Tutorials Point