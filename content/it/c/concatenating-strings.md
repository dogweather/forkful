---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:34:14.953006-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Unire le stringhe, o concatenazione, significa attaccare una stringa alla fine di un'altra. Si fa per creare nuovi messaggi o per formati dati da visualizzare o elaborare.

## How to:
In C, uniamo le stringhe usando la funzione `strcat()` della standard library. È semplice ma attento: bisogna avere spazio a sufficienza nel primo array di caratteri.

```C
#include <stdio.h>
#include <string.h>

int main() {
    char saluto[20] = "Ciao ";
    const char *nome = "Mondo!";
    
    strcat(saluto, nome); // Attacchiamo "Mondo!" a "Ciao "
    
    printf("%s\n", saluto); // Stampa "Ciao Mondo!"
    
    return 0;
}
```

## Deep Dive
Concatenare stringhe è un must da quando si scrive codice. Prima, si lavorava con i singoli caratteri, unendo manualmente. Oggi, funzioni come `strcat()` e `strncat()` semplificano il processo, ma c'è sempre il rischio di sovrascrivere la memoria se non si presta attenzione alle dimensioni dell'array di destinazione.

Alternative? `sprintf()` è versatile per unire e formattare testo. Implementazioni manuali con loop `for` o `while` danno più controllo ma richiedono più codice e attenzione agli errori.

Sul lato implementazione, `strcat()` funziona così:
1. Cerca il terminatore NULL della prima stringa.
2. Inizia a copiare la seconda stringa da quel punto fino al suo terminatore NULL.

Ricorda, la sicurezza prima di tutto: `strncat()` limita i caratteri copiati, prevenendo i buffer overflow, un tipo comune di vulnerabilità di sicurezza.

## See Also
- The C Standard Library documentation su `strcat()` e `strncat()`: http://www.cplusplus.com/reference/cstring/strcat/
- Sicurezza durante la manipolazione di stringhe in C: https://owasp.org/www-community/vulnerabilities/Buffer_Overflow