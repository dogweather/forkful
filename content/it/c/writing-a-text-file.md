---
title:                "Scrivere un file di testo"
html_title:           "C: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Quando lavori con il linguaggio di programmazione C, potresti dover scrivere un file di testo per archiviare i dati o per comunicare con altri programmi. Questo articolo ti mostrerà come fare.

## Come Fare

Scrivere un file di testo in C è un processo semplice e diretto. Ecco un esempio di come farlo utilizzando le funzioni standard della libreria `stdio.h`:

```C
#include <stdio.h>

int main() {
    // Apriamo un file di testo in modalità scrittura ('w')
    FILE *file = fopen("sample.txt", "w");

    // Scriviamo alcune righe di testo nel file
    fprintf(file, "Ciao, questo è un file di testo scritto in C.\n");
    fprintf(file, "Puoi anche scrivere variabili intere come questa: %d\n", 42);
    fprintf(file, "Ogni riga viene aggiunta successivamente al file.\n");

    // Chiudiamo il file
    fclose(file);
    
    // Nota: Controlla sempre se il file viene aperto correttamente e gestisci gli eventuali errori
    return 0;
}
```

L'output del programma creerà un file di testo chiamato "sample.txt" con le righe di testo scritte all'interno. Puoi modificare il nome del file e il contenuto in base alle tue esigenze.

## Deep Dive

Oltre alle funzioni standard della libreria `stdio.h`, puoi utilizzare anche le funzioni `fputc()` e `fputs()` per scrivere caratteri e stringhe rispettivamente. Inoltre, puoi anche utilizzare la modalità "append" (`"a"`) invece di "write" (`"w"`) per aggiungere nuove linee di testo a un file già esistente.

## Vedi Anche

Alcuni articoli correlati che possono essere utili:

- [Lettura di file di testo in C](https://www.programmareinc.it/c-file-text-reading)
- [Funzioni standard di I/O in C](https://riptutorial.com/c/topic/338/input-output)
- [Tutorial su C per principianti](https://www.learn-c.org/)