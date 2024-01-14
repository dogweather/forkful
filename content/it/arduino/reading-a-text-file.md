---
title:    "Arduino: Lettura di un file di testo"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

La lettura di un file di testo è un'operazione comune nella programmazione di Arduino. Questo può essere utile per leggere dati o configurazioni da un file, riducendo la quantità di codice necessaria per implementare tali funzionalità direttamente nel programma.

## Come fare

Per leggere un file di testo in Arduino, il primo passo è aprire il file utilizzando la funzione `SD.open()`. Questo richiederà il percorso del file e specificare la modalità di lettura (lettura, scrittura, ecc). Quindi, possiamo utilizzare un loop `while` per leggere il contenuto del file e salvare i dati in una variabile utilizzando i metodi della classe `File`.

```Arduino
File myFile = SD.open("nomefile.txt", FILE_READ); // Apri il file in modalità lettura

if(myFile){ // Controllo se il file è stato aperto con successo
  while(myFile.available()){ // Continua finché c'è del contenuto da leggere
    char data = myFile.read(); // Leggi un carattere dal file e salvalo in una variabile
    Serial.println(data); // Stampa il carattere sulla console seriale
  }

  myFile.close(); // Chiudi il file una volta completata la lettura
 }
```

L'esempio sopra leggerà il contenuto di `nomefile.txt` e lo stamperà sulla console seriale. È importante ricordare di chiudere il file dopo averlo utilizzato per evitare eventuali problemi di memoria.

## Approfondimento

Quando si lavora con file di testo, è importante conoscere i diversi metodi e funzioni della classe `File` di Arduino. Ad esempio, è possibile utilizzare il metodo `File.seek()` per spostare il cursore di lettura in una posizione specifica all'interno del file. Questo può essere molto utile quando si lavora con dati strutturati all'interno del file.

Inoltre, è possibile scrivere su file utilizzando il metodo `File.println()` o `File.write()`, che funzionano in modo simile alla loro controparte di lettura. È importante notare che quando si scrive su un file, verrà sovrascritto il contenuto precedente, a meno che non si utilizzi il metodo `File.available()` per posizionare il cursore di scrittura in una posizione specifica.

## Vedi anche

- [Documentazione ufficiale di Arduino sulla classe File](https://www.arduino.cc/en/Reference/File)
- [Esempio di lettura file su Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIFile)
- [Tutorial più avanzato sulla lettura e scrittura di file su Arduino](https://www.toptal.com/arduino/arduino-reading-and-writing-text-file-tutorial)