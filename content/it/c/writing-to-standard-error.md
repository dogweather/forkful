---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error, o stderr, permette di segnalare errori e log senza intasare l'output standard dei programmi. I programmatori lo fanno per separare l'output normale dagli avvisi e dalle segnalazioni di errore, rendendo più semplice il debug e il monitoraggio dei processi.

## How to:
Usa `fprintf` o `fputs` per scrivere su stderr. Esempio:

```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Questo è un errore!\n");
    fputs("Errore con fputs!\n", stderr);
    return 0;
}
```

Output:
```
Questo è un errore!
Errore con fputs!
```

## Deep Dive:
`stderr` è arrivato con lo standard C ANSI nel 1989, essendo uno dei tre stream di I/O predefiniti. Alternativamente, per output non critici si può usare `stdout`, mentre `stderr` è non-buffered e assicura che l'errore sia stampato immediatamente. Implementandolo, ricorda che scrivere su `stderr` può essere rediretto o filtrato separatamente dall'output principale del programma.

## See Also:
Per maggiori informazioni e approfondimenti:
- La documentazione ufficiale di C su `stderr`: https://en.cppreference.com/w/c/io/std_streams
- Best practice per la gestione degli errori in C: https://www.gnu.org/prep/standards/html_node/Errors.html