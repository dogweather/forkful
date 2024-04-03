---
date: 2024-01-20 17:39:31.378962-07:00
description: "How to: Arduino usa una SD card per la gestione dei file. Per creare\
  \ un file temporaneo, connettete prima la SD card, poi usate queste funzioni nel\
  \ vostro\u2026"
lastmod: '2024-03-13T22:44:43.702654-06:00'
model: gpt-4-1106-preview
summary: Arduino usa una SD card per la gestione dei file.
title: Creazione di un file temporaneo
weight: 21
---

## How to:
Arduino usa una SD card per la gestione dei file. Per creare un file temporaneo, connettete prima la SD card, poi usate queste funzioni nel vostro sketch:

```Arduino
#include <SPI.h>
#include <SD.h>

File tempFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Errore nell'inizializzazione della SD");
    return;
  }
  tempFile = SD.open("temp.txt", FILE_WRITE);
  if (tempFile) {
    Serial.println("File temporaneo creato");
    tempFile.println("Dati temporanei...");
    
    // Chiudete il file quando avete finito
    tempFile.close();
  } else {
    Serial.println("Errore nella creazione del file");
  }
}

void loop() {
  // Logica del programma...
}
```

Output previsto sul monitor seriale:
```
File temporaneo creato
```

## Deep Dive
L'uso di file temporanei su Arduino risale all'introduzione dell'SD library, che rese possibile la memorizzazione non volatile. Essenzialmente, quando si crea un file temporaneo su una SD, ci si assicura che i dati non affollino la memoria del microcontroller. Opzioni alternative includono l'uso della EEPROM interna di Arduino per dati piccoli o l'integrazione di un'altra forma di storage come EEPROM esterne o moduli memoria flash. Da tenere a mente: la SD può avere un numero limitato di cicli di scrittura; dunque, scrivere ripetutamente file temporanei può ridurne la durata.

## See Also
- Documentazione ufficiale SD library: https://www.arduino.cc/en/Reference/SD
- Forum Arduino per problemi specifici: https://forum.arduino.cc/
- Guida su EEPROM su Arduino: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
