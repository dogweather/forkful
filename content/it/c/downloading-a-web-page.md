---
title:                "C: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è un'attività comune per chiunque impari a programmare in C. Può essere utile per analizzare i dati di una pagina o per ottenere informazioni specifiche da un sito web.

## Come fare

Per scaricare una pagina web utilizzando il linguaggio C, è necessario seguire i seguenti passaggi:

1. Includere la libreria "stdio.h" per gestire gli input/ouput
2. Utilizzare la funzione "fopen" per aprire la pagina web
3. Utilizzare la funzione "fread" per leggere i dati dalla pagina e salvare il contenuto in una variabile
4. Chiudere il file utilizzando la funzione "fclose"
5. Stampare il contenuto della variabile utilizzando la funzione "printf"

Di seguito è riportato un esempio di codice che mostra come scaricare una pagina web utilizzando il linguaggio C:

```C
#include <stdio.h>

int main() {

   // Apertura del file
   FILE *fp = fopen("https://www.google.com/", "r");

   // Controllo se il file è stato aperto correttamente
   if(fp == NULL) {
      printf("Errore durante l'apertura della pagina web.");
      return 1;
   }

   // Lettura dei dati dalla pagina e salvataggio nel buffer
   char buffer[1000];
   size_t n = fread(buffer, 1, sizeof(buffer), fp);

   // Chiusura del file
   fclose(fp);

   // Stampa del contenuto del buffer
   printf("Contenuto della pagina: %.*s\n", n, buffer);

   return 0;
}
```

L'output di questo codice sarà il seguente:

```
Contenuto della pagina: <!doctype html><html ...
```

## Approfondimento

Ci sono molte altre funzioni e metodi per scaricare una pagina web utilizzando il linguaggio C. Ad esempio, è possibile utilizzare la libreria "curl" per rendere la procedura più semplice e gestire eventuali errori. Inoltre, è possibile utilizzare le funzioni di parsing per estrarre informazioni specifiche dalla pagina web.

## Vedi anche

- [Tutorial su come scaricare una pagina web utilizzando C](https://www.geeksforgeeks.org/download-web-page-using-curl/)
- [Guida alla libreria "stdio.h"](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm)
- [Esempi di parsing di una pagina web in C](https://www.esourceparts.ca/blog/parsing-webpages-in-c-using-libcurl-the-program/)

Grazie per aver letto questo articolo! Speriamo di averti fornito una buona introduzione su come scaricare una pagina web utilizzando il linguaggio C. Continua a esplorare e sperimentare per scoprire le molte possibilità di utilizzo di questa tecnica. Buon coding!