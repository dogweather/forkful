---
title:                "Scaricare una pagina web"
html_title:           "C: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato a sviluppare siti web o applicazioni che richiedono di scaricare pagine web, la conoscenza del linguaggio C è fondamentale. Con la sua velocità e la sua potenza, C è uno strumento ideale per automatizzare il processo di download di pagine web.

## Come Fare

Un modo semplice per scaricare una pagina web utilizzando il linguaggio C è utilizzare la funzione `fopen()` per aprire un file e la funzione `fgets()` per leggere il contenuto della pagina linea per linea.

`` `C
#include <stdio.h>

int main() {
    // Apro il file da creare ed il file da cui scaricare
    FILE *file = fopen("index.html", "w");
    FILE *webpage = fopen("https://www.esempio.com/", "r");

    // Controllo se il file è stato aperto correttamente
    if (file == NULL || webpage == NULL) {
        printf("Errore nell'apertura dei file");
        return 1; // Errore
    }

    // Leggo la pagina linea per linea e la scrivo nel file
    char buffer[BUFSIZ];
    while (fgets(buffer, BUFSIZ, webpage) != NULL) {
        fputs(buffer, file);
    }

    // Chiudo i file
    fclose(file);
    fclose(webpage);

    return 0; // Tutto ok
}
`` `

L'esempio qui sopra utilizza le funzioni `fopen()` e `fgets()` per aprire un file, scaricare una pagina web e scrivere il contenuto della pagina nel file. Questo è solo uno dei modi in cui è possibile utilizzare il linguaggio C per scaricare pagine web, ma è un buon punto di partenza per approfondire ulteriormente.

## Approfondimento

Se vuoi approfondire ulteriormente la conoscenza sul download di pagine web utilizzando il linguaggio C, ecco alcuni suggerimenti per continuare la tua ricerca:

- [Differenza tra fopen, fclose, fwrite e fprintf in C](https://www.studytonight.com/c/file-input-output-in-c.php)
- [Utilizzo avanzato delle funzioni di memoria in C](https://c-faq.com/decl/multiargi.html)
- [Come gestire gli errori nella programmazione C](https://www.codeproject.com/Articles/695869/Exception-Handling-in-C-The-Right-Way)

## Vedi Anche
- [Documentazione ufficiale di standard C](https://en.cppreference.com/w/c)
- [Esempi di codice C](https://www.programiz.com/c-programming/examples)
- [Tutorial sul linguaggio C per principianti](https://www.learn-c.org/)