---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perchè?
Eliminare i caratteri che corrispondono a un modello è un processo di manipolazione delle stringhe in cui si rimuovono certi caratteri da una stringa. I programmatori lo fanno per pulire o standardizzare i dati di input.

## Come Fare:
Qui un esempio tedesco di come eliminare tutti i caratteri 'c' da una stringa:

```C
#include <stdio.h>
#include <string.h>

void elimina_carattere(char *str, char c) {
    int i = 0;
    int j = 0;
    while (str[i]) {
        if (str[i] != c) {
            str[j++] = str[i];
        }
        i++;
    }
    str[j] = '\0';
}

int main() {
    char stringa[] = "cciao mondo c!";
    elimina_carattere(stringa, 'c');
    printf("%s\n", stringa);  // output: "iao mondo !"
    return 0;
}
```
In questo esempio, la funzione `elimina_carattere` rimuove tutti gli 'c' dalla stringa.

## Approfondimento
La necessità di eliminare i caratteri corrispondenti a un modello è nata con l'elaborazione del testo nei primi giorni delle scienze informatiche. La versione C della funzione di eliminazione è piuttosto semplice e diretta, utilizzando solo le funzioni di libreria standard.

Come alternative, potresti usare una funzione ricorsiva per eliminare i caratteri. Tuttavia, la versione iterativa è generalmente più efficiente in termini di memoria.

L'implementazione di questa funzione lavora scorrendo la stringa e sovrascrivendo i caratteri che non corrispondono al carattere da rimuovere. Alla fine, termina la stringa con un carattere nullo.

## Guarda Anche
-www.stackoverflow.com: For discussions on real-world applications and variants of this function
-www.cplusplus.com: For more examples and explanation of the string manipulation functions.