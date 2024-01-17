---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa e Perche?
Scrivere un file di testo è un'operazione comune per i programmatori. Si tratta di creare un documento contenente informazioni in formato di testo, che può essere letto e manipolato dal computer. I programmatori spesso scrivono file di testo per immagazzinare dati o per comunicare con altri programmi.

## Come fare:
Per scrivere un file di testo con Arduino, puoi utilizzare la funzione `File.print()` o `File.println()`. Ad esempio, per scrivere una stringa di testo all'interno di un file chiamato "dati.txt", puoi utilizzare il seguente codice:
```Arduino
File file = SD.open("dati.txt", FILE_WRITE);
if(file) {
  file.println("Ciao Mondo!");
  file.close();
}
```
Il metodo `File.println()` aggiunge una nuova riga alla fine del file e il metodo `file.close()` chiude il file dopo aver scritto nel suo interno.

## Approfondimento:
Scrivere file di testo è una tecnica comune anche in altri linguaggi di programmazione, come ad esempio il C++. In alternativa alle funzioni `File.print()` e `File.println()`, è possibile utilizzare la libreria "SD" per accedere alla scheda SD su cui scrivere i file. Ad esempio, per scrivere una stringa nel file "dati.txt" utilizzando la libreria "SD", il codice sarà il seguente:
```Arduino
#include <SD.h>
File file = SD.open("dati.txt", FILE_WRITE);
if(file) {
  file.println("Ciao Mondo!");
  file.close();
}
```
La libreria "SD" offre anche varie opzioni per il controllo dell'accesso al file e la gestione di eventuali errori durante la scrittura.

## Vedi anche:
Per una guida dettagliata sulle funzioni di scrittura dei file di testo in Arduino, controlla la documentazione ufficiale su [File.write()](https://www.arduino.cc/reference/en/language/functions/communication/file/write/) e [File.println()](https://www.arduino.cc/reference/en/language/functions/communication/file/println/). Inoltre, puoi trovare ulteriori informazioni sull'utilizzo delle schede SD con Arduino su [SD Library reference](https://www.arduino.cc/en/Reference/SD).