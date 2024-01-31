---
title:                "Gestione degli errori"
date:                  2024-01-26T00:37:16.459097-07:00
model:                 gpt-4-1106-preview
simple_title:         "Gestione degli errori"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/handling-errors.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Gestire gli errori in C significa aspettarsi l'inaspettato. Impedisce ai programmi di andare in tilt quando incontrano problemi. I programmatori lo fanno per gestire gli errori con eleganza e mantenere il loro codice affidabile.

## Come fare:

Vediamo come farlo in C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("nonexistentfile.txt", "r");
    if (fp == NULL) {
        perror("Errore nell'apertura del file");
        return EXIT_FAILURE;
    }
    // Fai qualcosa con il file
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Output di esempio quando il file non esiste:
```
Errore nell'apertura del file: Nessun file o directory
```

## Approfondimento

Nei primi tempi del C, la gestione degli errori era essenziale - per lo più codici di ritorno e controlli manuali. Da qui `errno`, una variabile globale aggiornata quando le funzioni falliscono. Non è thread-safe di per sé, perciò sono state introdotte le funzioni `strerror` e `perror` per una migliore segnalazione degli errori.

Alternative? Il C moderno non è limitato ad `errno`. Ci sono setjmp e longjmp per salti non-locali quando si verifica un disastro. Alcuni preferiscono definire i propri codici di errore, mentre altri optano per strutture simili alle eccezioni in C++.

I dettagli implementativi possono essere complessi. Ad esempio, `errno` è thread-safe nei sistemi conformi a POSIX grazie alla magia del Thread Local Storage (TLS). Nei sistemi embedded, dove le risorse sono preziose, il codice di gestione degli errori personalizzato potrebbe essere preferito rispetto agli approcci standard che potrebbero appesantire il software.

## Vedi anche

- Un'analisi dettagliata di `errno`: https://en.cppreference.com/w/c/error/errno
- Per la sicurezza dei thread, vedi i thread POSIX e errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Un'introduzione a setjmp e longjmp: https://www.cplusplus.com/reference/csetjmp/
- Per la gestione delle eccezioni in C++, consulta: https://isocpp.org/wiki/faq/exceptions
