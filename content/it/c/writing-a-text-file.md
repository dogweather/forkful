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

Che cos'è e perché?

Scrivere un file di testo in C è un'attività essenziale per i programmatori. Consiste nel creare un file di testo che può essere letto e modificato dal tuo programma. Questo può essere utile per memorizzare informazioni importanti o per tenere traccia dei dati del tuo programma.

Come fare:

Per scrivere un file di testo in C, è necessario utilizzare le funzioni di libreria standard di C. Ecco un esempio di codice che ti mostra come creare un file di testo e scrivere alcune righe in esso:

```C
#include <stdio.h>

int main() {
   FILE *fp;

   fp = fopen("test.txt", "w");
   fprintf(fp, "Questo è un esempio di scrittura su file di testo.\n");
   fprintf(fp, "Puoi scrivere il tuo contenuto qui.");
   fclose(fp);
   return 0;
}
```
Output:

Il codice sopra creerà un file di testo chiamato "test.txt" e scriverà due righe all'interno. Puoi aprire il file per verificare il contenuto scritto.

Deep Dive:

Lo scrivere su file è una pratica comune nella programmazione. I file di testo possono essere utilizzati per salvare informazioni di configurazione, dati di output e altri dati importanti del tuo programma. In passato, i file di testo erano spesso utilizzati come unico modo per memorizzare dati, ma ora ci sono altre opzioni come i database o i servizi cloud.

Vedi anche:

- [Esempi di scrittura su file di testo in C] (https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Documentazione ufficiale delle funzioni di libreria standard di C] (https://www.gnu.org/software/libc/manual/html_node/File-Access-Permission.html#File-Access-Permission)
- [Un esempio di lettura e scrittura di un file di testo in C] (https://www.programiz.com/c-programming/c-file-input-output)

# Note sull'edizione

Questa è la versione corrente di C, chiamata anche C18. È stata pubblicata nel dicembre 2018 ed è l'ultima versione del linguaggio al momento della scrittura di questo articolo. Assicurati sempre di utilizzare l'ultima versione disponibile per trarre vantaggio dalle ultime funzionalità e miglioramenti.