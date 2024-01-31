---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-19
simple_title:         "Verifica dell'esistenza di una directory"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Nella programmazione Arduino, controllare l'esistenza di una directory è una verifica per sapere se un certo percorso di memoria è accessibile o meno, di solito su una scheda SD. Si fa per evitare errori nei file o nei dati durante la lettura o la scrittura.

## Come Fare:
Per controllare se una directory esiste su una scheda SD, serve la libreria `SD.h`. Ecco un esempio di codice:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // aspetta la connessione seriale
  }

  if (!SD.begin(4)) {
    Serial.println("Inizializzazione SD fallita!");
    return;
  }

  if (SD.exists("/example")) {
    Serial.println("La directory esiste!");
  } else {
    Serial.println("La directory non esiste.");
  }
}

void loop() {
  // qui il codice che si ripeterà
}
```
Output:
```
La directory esiste!
```
o
```
La directory non esiste.
```

## Approfondimento
La possibilità di controllare l'esistenza di una directory risale ai primi sistemi operativi. Su Arduino, questa funzionalità diventa critica quando si interagisce con la memoria esterna, come le schede SD. Alternative di verifica comprendono la creazione di una nuova directory se non esiste con `SD.mkdir("/nuovaDir")`. Sul fronte implementativo, Arduino usa internamente il sistema di file FAT16 o FAT32, quindi l'esistenza della directory è determinata secondo questi standard.

## Vedi Anche
- Documentazione ufficiale della libreria SD di Arduino: [Arduino - SD](https://www.arduino.cc/en/Reference/SD)
- Tutorial sull'uso della scheda SD con Arduino: [Adafruit SD Tutorial](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial)
- Specifiche del sistema di file FAT: [Microsoft FAT Specification](https://docs.microsoft.com/en-us/windows/win32/fileio/exfat-specification)
