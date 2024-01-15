---
title:                "Ricerca e sostituzione di testo"
html_title:           "C: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché
Ci sono diverse ragioni per cui potresti voler cercare e sostituire del testo in un programma scritto in C. Forse hai scoperto un bug nella tua applicazione che richiede una correzione in molti punti del codice, o forse vuoi semplicemente aggiornare una determinata stringa con una nuova. Cercare e sostituire è uno strumento utile per semplificare queste operazioni ripetitive e garantire la coerenza nel codice.

## Come Fare
Per cercare e sostituire il testo in un programma C, puoi utilizzare la funzione `str_replace()` della libreria standard `string.h`. Questa funzione accetta tre argomenti: la stringa originale, la stringa da cercare e la stringa da sostituire. Di seguito è riportato un esempio di codice che utilizza la funzione `str_replace()` per sostituire "ciao" con "salve" nella stringa `str`:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[20] = "ciao mondo";
    char *new_str = str_replace(str, "ciao", "salve");
    printf("%s", new_str);
    return 0;
}
```

Questo codice stamperà "salve mondo" sulla console. Ricorda che la funzione `str_replace()` non modifica la stringa originale, ma restituisce una nuova stringa con le modifiche apportate.

## Approfondimento
Per coloro che sono interessati ad approfondire il concetto di ricerca e sostituzione di testo in un programma scritto in C, vale la pena esplorare anche la funzione `strtok()` della libreria `string.h`. Questa funzione divide una stringa in "token" separati da un delimitatore. Se il delimitatore è un carattere che desideri sostituire, puoi utilizzare `strtok()` per separare la stringa originale nei punti in cui vuoi effettuare la sostituzione. Di seguito è riportato un esempio di codice che utilizza `strtok()` per sostituire ogni spazio in una stringa con un carattere "x":

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[20] = "ciao mondo";
    char *token = strtok(str, " ");
    while (token != NULL) {
        printf("%s", token);
        token = strtok(NULL, " ");
    }
    return 0;
}
```

Questo codice stamperà "ciaoxmondo" sulla console. Ricorda che `strtok()` modificherà la stringa originale, quindi è importante fare attenzione all'utilizzo di questa funzione.

## Vedi Anche
- Funzione `str_replace()` nella documentazione di C della GNU
- Funzione `strtok()` nella documentazione di C della GNU