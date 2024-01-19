---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Creare un file temporaneo significa generare un file a uso provvisorio per una sessione di lavoro. I programmatori lo fanno per manipolare brevemente i dati senza modificare il file originale.

## Come fare:

Ecco un esempio su come creare un file temporaneo con Arduino.

```Arduino
#include <SD.h>

File fileTemp;

void setup() {
  Serial.begin(9600);
  SD.begin(4);

  fileTemp = SD.open("fileTemp.txt", O_WRITE | O_CREAT | O_TRUNC);
  
  if (fileTemp) {
    fileTemp.println("Questo è un file temporaneo");
    fileTemp.close();
    Serial.println("File temporaneo creato correttamente");
    
    } else {
    Serial.println("Errore nella creazione del file temporaneo");
  }
}

void loop() {
  // non fa nulla
}
```
In uscita vedrai:

```Arduino
File temporaneo creato correttamente
```

## Approfondimento:

La creazione di file temporanei è una pratica antica quanto l'informatica. Al giorno d'oggi, molte librerie software disponibili per Arduino consentono di eseguire tale operazione.

Un'alternativa potrebbe essere l'uso di variabili temporaie se il volume dei dati non è eccessivo.

Inoltre, è importante notare che i file temporanei solo consumano spazio nell'SD. Una volta che hai finito di usarlo, è un buon pratica eliminare il file.

## Guarda Anche: 

1. La documentazione ufficiale SD Arduino Library: https://www.arduino.cc/en/reference/SD
2. Un'ottima guida su come utilizzare la SD con Arduino: https://www.makerguides.com/sd-card-arduino-tutorial/