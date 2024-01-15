---
title:                "Crea un file temporaneo"
html_title:           "Arduino: Crea un file temporaneo"
simple_title:         "Crea un file temporaneo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché creare un file temporaneo su Arduino?

Creare un file temporaneo su Arduino può essere utile in molte situazioni, ad esempio per salvare dati temporanei o per creare un sistema di recupero dati in caso di malfunzionamenti.

## Come creare un file temporaneo su Arduino

La funzione `File::createTempFile()` consente di creare un file temporaneo su Arduino.

```
Arduino
#include <SD.h>

void setup() {
  SD.begin(10);
  
  File tempFile = SD.open("temp.txt", FILE_WRITE);
  tempFile.println("Questo è un file temporaneo!");
  tempFile.close();
}
```

Una volta compilato e caricato questo codice su Arduino, verrà creato un file temporaneo chiamato "temp.txt" contenente il testo "Questo è un file temporaneo!".

## Approfondimento sulla creazione di un file temporaneo su Arduino

La funzione `File::createTempFile()` accetta due parametri: il primo è il nome del file (nella nostra esempio "temp.txt") e il secondo è la modalità di scrittura del file (ad esempio `FILE_WRITE` o `FILE_READ`).

Inoltre, è importante notare che i file temporanei creati con questa funzione vengono automaticamente eliminati quando si spegne Arduino o si riavvia il programma.

## Vedi anche

- Documentazione ufficiale di Arduino su creazione di file temporanei: https://www.arduino.cc/reference/en/libraries/sd/file/createtempfile/
- Esempi di codice per la gestione dei file su Arduino: https://github.com/arduino-libraries/SD/tree/master/examples