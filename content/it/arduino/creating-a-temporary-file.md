---
title:    "Arduino: Creazione di un file temporaneo"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Molte volte durante lo sviluppo di progetti Arduino, ci troviamo nella necessità di creare dei file temporanei per memorizzare dati o per eseguire operazioni. Creare un file temporaneo può sembrare un'operazione complicata, ma in realtà è abbastanza semplice e può semplificare il lavoro del programmatore.

## Come fare

Per creare un file temporaneo in Arduino, dobbiamo seguire questi semplici passaggi:

1. Includere la libreria "SD.h" nel nostro codice.
2. Definire una variabile di tipo "File".
3. Utilizzare la funzione "SD.open" per creare il file temporaneo nella scheda SD.
4. Scrivere e leggere dati dal file temporaneo utilizzando le funzioni "write" e "read" della libreria "SD".

```Arduino
#include <SD.h> // inclusione della libreria SD

File tempFile; // definizione di una variabile di tipo File

void setup() {
  // inizializzazione della scheda SD
  if(!SD.begin(10)) {
    Serial.println("Errore durante l'inizializzazione della scheda SD!");
    while(1);
  }
  // apertura del file temporaneo
  tempFile = SD.open("temp.txt", FILE_WRITE);
  
  // scrittura di un dato nel file
  tempFile.write("Ciao mondo!");
  
  // chiusura del file
  tempFile.close();
}

void loop() {
  // riapertura del file
  tempFile = SD.open("temp.txt", FILE_READ);
  
  // lettura dei dati dal file e stampa su monitor seriale
  while(tempFile.available()) {
    Serial.write(tempFile.read());
  }
  
  // chiusura del file
  tempFile.close();
  
  // attesa di 1 secondo
  delay(1000);
}
```

Output:

```
Ciao mondo!
```

## Approfondimenti

Oltre all'utilizzo della libreria "SD.h" per creare file temporanei in Arduino, possiamo anche utilizzare altre librerie come "SPIFFS.h" o "EEPROM.h". Inoltre, possiamo gestire i file temporanei in modo avanzato, ad esempio eliminandoli dopo l'utilizzo o gestendo la loro dimensione.

## Vedi anche

- [Libreria SD.h](https://www.arduino.cc/en/Reference/SD)
- [Libreria SPIFFS.h](https://www.arduino.cc/en/Reference/SPIFFS)
- [Libreria EEPROM.h](https://www.arduino.cc/en/Reference/EEPROM)