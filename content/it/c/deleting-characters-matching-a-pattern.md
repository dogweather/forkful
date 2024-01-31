---
title:                "Eliminazione di caratteri che corrispondono a un pattern"
date:                  2024-01-20T17:41:36.627736-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminazione di caratteri che corrispondono a un pattern"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Cancellare caratteri che corrispondono ad un pattern significa rimuovere specifici caratteri da una stringa - una prassi comune per pulire i dati o per formati di input. Programmatori lo fanno per validare input, semplificare elaborazioni successive, o per sicurezza.

## How to:
```C
#include <stdio.h>
#include <string.h>

void elimina_pattern(char *sorgente, const char *pattern) {
    char *i = sorgente, *j = sorgente;
    while (*j != '\0') {
        // Copia solo i caratteri che non sono nel pattern
        if (strchr(pattern, *j) == NULL) {
            *i++ = *j;
        }
        j++;
    }
    *i = '\0'; // Terminate stringa pulita
}

int main() {
    char testo[] = "Ciao mondo! Questo e' un test.";
    elimina_pattern(testo, "!'");
    printf("Testo pulito: %s\n", testo);
    return 0;
}
```
Output:
```
Testo pulito: Ciao mondo Questo e un test.
```
Questo codice fa scomparire ogni punto esclamativo e apostrofo dal testo.

## Deep Dive:
Nel 1972, Dennis Ritchie crea il linguaggio C, e da allora gestire stringhe è fondamentale. Un'alternativa alla funzione `elimina_pattern` potrebbe essere l'uso delle regex (espressioni regolari), ma in C standard devi integrare librerie aggiuntive per farlo. L'implementazione illustrata è semplice e diretta: scorre ogni carattere e controlla se fa parte del pattern da eliminare, conservando il resto.

## See Also:
- [C String Library](http://www.cplusplus.com/reference/cstring/)
- [C Standard Library Functions](https://en.cppreference.com/w/c/string/byte)
- [Regular Expressions with POSIX](https://pubs.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap09.html)
