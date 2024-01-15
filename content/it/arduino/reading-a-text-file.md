---
title:                "Leggere un file di testo"
html_title:           "Arduino: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se hai bisogno di leggere un file di testo utilizzando Arduino, sei nel posto giusto! In questo articolo imparerai come farlo in modo semplice e veloce.

## Come fare

Innanzitutto, assicurati di avere un file di testo che vuoi leggere sul tuo computer. Puoi crearne uno utilizzando un editor di testo come Blocco Note o WordPad. Salva il file con un nome facile da ricordare e un'estensione .txt.

Una volta che hai il tuo file di testo pronto, collega Arduino al tuo computer e apri l'IDE di Arduino. Ora vediamo come leggere il file di testo utilizzando il codice.

```
Arduino #include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while(!SD.begin(4)) {
    Serial.println("Inserisci una scheda SD.");
    delay(1000);
  }

  myFile = SD.open("file.txt", FILE_READ);

  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("Errore nell'apertura del file.");
  }
}

void loop() {}
```

In questo codice, abbiamo incluso due librerie: SPI.h e SD.h. Dobbiamo anche creare un oggetto tramite il quale apriremo il nostro file di testo. Verifichiamo inoltre che la scheda SD sia inserita prima di continuare con l'apertura del file. Se tutto va bene, leggiamo il contenuto del file e lo stampiamo nella console seriale. Se c'è un errore nell'apertura del file, stampiamo un messaggio di errore nella console.

## Approfondimento

In questo articolo, abbiamo visto come leggere un file di testo utilizzando Arduino. Tuttavia, ci sono molti altri modi per farlo, come utilizzare un lettore di schede SD esterno o utilizzare Ethernet Shield. Inoltre, puoi sperimentare con diverse funzioni per manipolare il contenuto del file di testo, come cercare una determinata parola o convertire i dati in un formato diverso. Non esitare a esplorare e divertirti con il codice!

## Vedi anche

- [Tutorial di Arduino: Come leggere dati da un file](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Libreria SD di Arduino](https://www.arduino.cc/en/Reference/SD)
- [Libreria SPI di Arduino](https://www.arduino.cc/en/Reference/SPI)