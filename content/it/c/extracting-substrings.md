---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:45:09.231965-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Estrarre sottostringhe significa catturare parti specifiche di una stringa di testo. I programmatori lo fanno per analizzare, modificare, o semplicemente visualizzare segmenti rilevanti dei loro dati di input.

## How to:
Estrarre sottostringhe in C si fa spesso con `strncpy`. Ma attenzione: `strncpy` non aggiunge automaticamente il carattere `\0` per terminare la stringa. Ecco come fare:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char text[] = "Ciao, mondo!";
    char dest[5]; // Scegli dimensione adeguata

    // Estrarre "Ciao"
    strncpy(dest, text, 4);
    dest[4] = '\0';   // Aggiungi il terminatore di stringa manualmente

    printf("La sottostringa è: '%s'\n", dest);
    return 0;
}
```

Output:
```
La sottostringa è: 'Ciao'
```

## Deep Dive:
L'estrazione di sottostringhe ha radici storiche nella necessità di manipolare testo limitando l'uso della memoria. Nei primi tempi, con meno memoria disponibile, ogni byte contava.
Alternative a `strncpy` includono `memcpy` e `strndup`. `memcpy` si usa quando si conosce la lunghezza del segmento di memoria e `strndup` duplica la sottostringa, occupandosi anche del `\0` finale.
L'implementazione varia considerando che nessuna funzione standard di C fornisce una soluzione completa per sottostringhe sicure - c'è sempre da pensare al terminatore di stringa.

## See Also:
- [`strncpy` reference](http://www.cplusplus.com/reference/cstring/strncpy/)
- [Manipolare stringhe in C - tutorial](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [`strndup` function](https://linux.die.net/man/3/strndup)
