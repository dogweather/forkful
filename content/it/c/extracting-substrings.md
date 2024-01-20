---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

L'estrazione di sottostringhe in C è l'atto di estrarre una parte di una stringa esistente per creare una nuova stringa. Gli sviluppatori lo fanno per lavorare con parti specifiche delle stringhe, senza dover gestire l'intera lunghezza della stringa.

## Come fare:

Ecco un esempio di come estrarre una sottostringa usando la funzione `strncpy` nel linguaggio C.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char sorgente[] = "Buongiorno, mondo!";
    char destinazione[15];

    strncpy(destinazione, sorgente, 10);
    destinazione[10] = '\0';  // End of string

    printf("%s\n", destinazione);  // Outputs: Buongiorno
    return 0;
}
```
In questo esempio, stiamo estraendo i primi 10 caratteri dalla stringa "Buongiorno, mondo!" e stiamo mettendo il risultato nella stringa di destinazione.

## Approfondimento

Estrarre una sottostringa è una tecnica comune in programmazione e contesti di elaborazione del testo. Questa pratica risale ai primi giorni di programmazione. Anche se la funzione `strncpy` è abbastanza diretta, è importante notare che non aggiunge automaticamente un carattere null alla fine della sottostringa, quindi è necessario farlo manualmente.

Ci sono alternative alla funzione `strncpy` come l'uso di puntatori per estrarre una sottostringa. Tuttavia, `strncpy` è considerato più sicuro poiché minimizza i problemi di overflow del buffer.

L'implementazione effettiva di `strncpy` può variare leggermente a seconda del compilatore o del sistema operativo, ma l'idea di base è la stessa.

## Vedere Anche

1. Documentazione ufficiale di `strncpy`: http://www.cplusplus.com/reference/cstring/strncpy/
2. Guida stringhe C sulla W3schools: https://www.w3schools.in/c-tutorial/strings/
3. Wikipedia, manipolazione delle stringhe: https://en.wikipedia.org/wiki/String_manipulation