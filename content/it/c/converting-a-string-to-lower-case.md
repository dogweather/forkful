---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La conversione di una stringa in minuscolo è un'operazione comune in programmazione che consiste nel cambiare tutte le lettere maiuscole in una stringa in lettere minuscole. Questo può essere utile per uniformare i dati, per esempio quando si confrontano due stringhe in modo insensibile al maiuscolo/minuscolo.

## Come fare:
La libreria delle stringhe standard in C include molte funzioni per manipolare le stringhe, tra cui `tolower()`. Di seguito è riportato un esempio:

```C
#include <ctype.h>
#include <stdio.h>
#include <string.h>

void strToLowerCase(char* s) {
    for(int i = 0; s[i]; i++){
        s[i] = tolower(s[i]);
    }
}

int main() {
    char str[] = "CIAO MONDO!";
    strToLowerCase(str);
    printf("%s\n", str);
    return 0;
}
```

Uscita:
```
ciao mondo!
```

## Approfondimento
Storicamente, la necessità di convertire le stringhe in minuscolo risale ai primi giorni della programmazione, quando l'elaborazione del testo divenne una pratica comune. Nel linguaggio C, la funzione `tolower()` è stata introdotta nella libreria standard C come uno dei metodi per la gestione delle stringhe.

Ci sono anche altri modi per convertire le stringhe in minuscolo. Un'altra funzione popolare è `strlwr()` nella libreria `string.h`, ma non è parte dello standard ANSI C, quindi potrebbe non essere disponibile su tutte le piattaforme.

In termini di implementazione, la funzione `tolower()` lavora per caratteri. Controlla se il carattere è una lettera maiuscola attraverso la tabella ASCII. Se lo è, lo converte in minuscolo aggiungendo 32 (la differenza tra maiuscole e minuscole nella tabella ASCII).

## Vedi anche
1. [Documentazione della funzione tolower](https://www.cplusplus.com/reference/cctype/tolower/)
3. [Tabella ASCII](https://www.asciitable.com/)
4. [Confronto di stringhe insensibile alla differenza tra maiuscole e minuscole in C](https://stackoverflow.com/questions/5820810/case-insensitive-string-comp-in-c)