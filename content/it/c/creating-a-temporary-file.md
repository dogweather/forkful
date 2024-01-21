---
title:                "Creazione di un file temporaneo"
date:                  2024-01-20T17:39:50.258262-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creazione di un file temporaneo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Creare un file temporaneo è l'atto di generare un file destinato a essere utilizzato per un breve periodo, spesso per lo storage di dati di lavoro. Programmatori lo fanno per gestire dati che non devono sopravvivere dopo la terminazione del programma, come i file di cache o per evitare conflitti di nomi in scenari multiutente o multithread.

## How to: (Come fare:)
In C, utilizzi la funzione `tmpfile()` per creare un file temporaneo che si chiude automaticamente quando il programma termina.

```C
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    if (temp) {
        // Fai qualcosa con il file temporary
        fputs("Ecco un esempio di file temporaneo.\n", temp);

        // Riporta il file pointer all'inizio del file
        rewind(temp);

        // Leggi dalla temporary file
        char buffer[256];
        while(fgets(buffer, sizeof(buffer), temp) != NULL) {
            printf("%s", buffer);
        }
        // Il file viene automaticamente chiuso alla fine del programma
    } else {
        perror("Alcuni errori sono successi nella creazione del temporary file");
    }
    return 0;
}
```

Output esempio:
```
Ecco un esempio di file temporaneo.
```

## Deep Dive (In Profondità)
I file temporanei sono parte di Unix fin dalla sua nascita negli anni '70. In C, la funzione `tmpfile()` standard ANSI C gestisce la creazione e apertura di un file temporaneo unico che è poi automaticamente rimosso quando il file è chiuso o quando il programma termina.

Alternativamente, puoi usare `mkstemp()` se hai bisogno di una maggiore controllo sul nome del file temporaneo, anche se dovrai gestire manualmente la rimozione del file quale non è più necessario.

`tmpfile()` crea il file temporaneo nella cartella indicata dalla variabile di ambiente `TMPDIR`. Se `TMPDIR` non è impostata, di solito usa `/tmp` o una directory simile, a seconda del sistema operativo. Il file è creato con permessi di lettura e scrittura solo per l'utente dell'applicazione, quindi gli altri non possono leggerlo.

## See Also (Vedi Anche)
- [tmpfile(3) - Linux Man Page](https://man7.org/linux/man-pages/man3/tmpfile.3.html)
- [ISO C documentation for tmpfile()](https://www.iso.org/standard/74528.html)
- [Secure Coding in C and C++: Temporary File Security](https://resources.sei.cmu.edu/library/asset-view.cfm?assetid=52019)