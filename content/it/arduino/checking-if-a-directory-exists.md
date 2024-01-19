---
title:                "Verifica se una directory esiste"
html_title:           "Arduino: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Controllare se una directory è esistente significa verificare se un dato percorso di cartella esiste nel tuo sistema file SD. Lo facciamo per evitare errori nel tentativo di aprire una directory che non esiste.

## Come fare:
Utilizziamo la funzione `SD.exists()` per verificare l'esistenza di una directory. Vediamo come:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("Initialisation failed!");
    while (1);
  }

  if (SD.exists("/example_directory")) {
    Serial.println("Directory exists.");
  } else {
    Serial.println("Directory doesn't exist.");
  }
}

void loop() {
  // nothing here
}
```
Ecco l'output:
```Arduino
Directory exists.
```
o:
```Arduino
Directory doesn't exist.
```

## Approfondimenti
Storicamente, `SD.exists()` è utilizzato ampiamente in Arduino per controllare l'esistenza di file e directory. Alternativamente, alcune librerie come `SdFat` offrono metodi simili. Però, `SD.exists()` ha un vantaggio significativo essendo incluso nelle librerie standard Arduino. È importante notare che, il metodo `SD.exists()` restituisce `true` anche se il percorso specificato è un file, non una directory.

## Vedi Anche
1. Documentazione Arduino sulla libreria SD: https://www.arduino.cc/en/Reference/SD
2. Guida dettagliata su SD.exists(): https://www.arduino.cc/en/Tutorial/LibraryExamples/Files
3. Documentazione sulla libreria SdFat: https://github.com/greiman/SdFat