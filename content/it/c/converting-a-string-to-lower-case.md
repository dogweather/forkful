---
title:                "Conversione di una stringa in minuscolo"
date:                  2024-01-20T17:37:46.063383-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabeticamente maiuscoli in minuscoli. I programmatori fanno ciò per uniformare i dati per confronti insensibili al caso, ordinamenti o per l'input utente standardizzato.

## How to:
Il C standard non fornisce una funzione diretta per convertire una stringa intera in minuscolo. Dobbiamo scrivere una funzione personalizzata. Ecco un esempio:

```C
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while(*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char testo[] = "CIAO MONDO!";
    toLowerCase(testo);
    printf("Testo convertito: %s\n", testo);
    return 0;
}

```

Output:
```
Testo convertito: ciao mondo!
```

## Deep Dive
La conversione da maiuscolo a minuscolo nel C esiste da quando il linguaggio è stato creato negli anni '70. Il C è un linguaggio povero di funzioni di alto livello per le stringhe, pertanto spesso i programmatori realizzano funzioni personalizzate.

In alternativa a `tolower`, per esempio, si potrebbe scrivere una funzione propria che manipola i codici ASCII direttamente, ma questa approccio non è portabile tra diverse codifiche caratteri.

Il `tolower` opera su un singolo carattere. Notare l'uso di `(unsigned char)` per evitare comportamenti non definiti se `char` è firmato e ha un codice negativo. Gli standard del C definiscono il comportamento di `tolower` solo con valori `unsigned char` e `EOF`.

Le soluzioni come la funzione `strlwr()` esistono in alcune librerie, ma non fanno parte dello standard C e quindi non sono portabili.

## See Also
Consulta i seguenti per approfondire:
- Reference di C Standard Library per `tolower`: https://en.cppreference.com/w/c/string/byte/tolower
- Stack Overflow su come convertire una stringa in minuscolo: https://stackoverflow.com/questions/2661766/how-do-i-lowercase-a-string-in-c
