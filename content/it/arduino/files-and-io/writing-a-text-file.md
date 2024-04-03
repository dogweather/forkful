---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:06.333749-07:00
description: "Scrivere un file di testo in Arduino implica salvare dati su un file\
  \ presente su una scheda SD o un modulo di memorizzazione simile, spesso a scopo\
  \ di\u2026"
lastmod: '2024-03-13T22:44:43.701671-06:00'
model: gpt-4-0125-preview
summary: Scrivere un file di testo in Arduino implica salvare dati su un file presente
  su una scheda SD o un modulo di memorizzazione simile, spesso a scopo di registrazione
  dati.
title: Scrivere un file di testo
weight: 24
---

## Come fare:
Per scrivere su un file di testo su una scheda SD utilizzando Arduino, è necessario prima includere la libreria `SD.h`, che fornisce le funzioni necessarie per interagire con le schede SD. Assicurati che la tua scheda Arduino sia collegata a un modulo di scheda SD.

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // Inizializza la comunicazione seriale a 9600 bit per secondo:
  Serial.begin(9600);
  
  // Verifica l'inizializzazione della scheda SD
  if (!SD.begin(4)) {
    Serial.println("Inizializzazione fallita!");
    return;
  }
  Serial.println("Inizializzazione completata.");
  
  // Apri il file. Nota che solo un file può essere aperto alla volta,
  // quindi devi chiudere questo prima di aprirne un altro.
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // Se il file è stato aperto correttamente, scrivi su di esso:
  if (myFile) {
    Serial.print("Scrittura su test.txt...");
    myFile.println("Test di scrittura file di testo.");
    // Chiudi il file:
    myFile.close();
    Serial.println("fatto.");
  } else {
    // Se il file non si apre, stampa un errore:
    Serial.println("Errore nell'apertura di test.txt");
  }
}

void loop() {
  // Dopo l'impostazione, non succede nulla
}
```

### Output Esempio:
Quando esegui questo codice, il Monitor Seriale dell'IDE Arduino visualizzerà:
```
Inizializzazione completata.
Scrittura su test.txt...fatto.
```
Per verificare se i dati sono stati scritti correttamente, puoi rimuovere la scheda SD dall'Arduino, inserirla in un computer e aprire il file `test.txt` per vedere il messaggio "Test di scrittura file di testo."

Per progetti che richiedono operazioni su file più avanzate o elaborazioni, considera l'esplorazione di ulteriori librerie o la scrittura di funzioni personalizzate adattate alle tue specifiche esigenze.
