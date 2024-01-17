---
title:                "Concatenando stringhe"
html_title:           "C: Concatenando stringhe"
simple_title:         "Concatenando stringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La concatenazione di stringhe è un concetto importante nella programmazione che implica la combinazione di due o più stringhe per crearne una nuova. Questo viene comunemente usato per creare output più significativi o per gestire input utente. È importante per i programmatori comprendere come concatenare stringhe per poter creare codice efficiente e flessibile.

## Come fare:

```
#include <stdio.h>

int main() {
    char str1[] = "Ciao";
    char str2[] = " mondo!";
    char output[13];
    // La nuova stringa avrà una dimensione totale di 13 caratteri
    sprintf(output, "%s%s", str1, str2);
    printf(output);
    // Output: Ciao mondo!
    return 0;
}
```

## Approfondimento:

La concatenazione di stringhe è stata introdotta per la prima volta nel linguaggio C e da allora è diventata una caratteristica fondamentale di molti linguaggi di programmazione. Alcune alternative alla concatenazione di stringhe includono l'utilizzo di puntatori o l'utilizzo di funzioni di libreria già esistenti per manipolare le stringhe. Per applicarla in modo efficace, è importante comprendere la differenza tra stringhe statiche e dinamiche e come allocare la memoria necessaria per una concatenazione di successo.

## Vedi anche:

- [Understanding String Concatenation in C](https://www.educba.com/string-concatenation-in-c/)
- [The Evolution of String Concatenation in Programming Languages](https://techbeacon.com/app-dev-testing/evolution-string-concatenation-programming-languages)