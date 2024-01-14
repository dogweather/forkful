---
title:                "Arduino: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Programmare con CSV (Comma Separated Values) può essere utile se si vuole integrare dati esterni nel proprio progetto Arduino. Questo tipo di file è molto diffuso e facile da leggere, permettendo una maggiore flessibilità nella gestione dei dati.

## Come fare

Per utilizzare CSV in un progetto Arduino, segue un esempio di codice per leggere e stampare i dati di un file CSV sulla porta seriale:

```Arduino
#include <SPI.h>
#include <SD.h>

File csvFile;
String data;

void setup() {
  Serial.begin(9600);
  
  // Inizializza la scheda SD
  if (!SD.begin(10, 11, 12, 13)) {
    Serial.println("Errore nell'inizializzazione della scheda SD");
    return;
  }

  // Apre il file CSV in lettura
  csvFile = SD.open("dati.csv");
  if (!csvFile) {
    Serial.println("Errore nell'apertura del file CSV");
    return;
  }
  
  // Cicla finché il file non è terminato
  while (csvFile.available()) {
    // Legge una riga del file
    data = csvFile.readStringUntil('\n');
    // Stampa la riga sulla porta seriale
    Serial.println(data);
  }
  // Chiude il file
  csvFile.close();
}

void loop() {
  // Codice di loop vuoto
}
```

L'output sarà simile a questo:

```
1. Ciao, mondo!
2. Questo è un esempio di file CSV.
3. È possibile inserire qualsiasi tipo di dato all'interno del file, anche numeri separati da virgola.
```

## Approfondimento

Il formato CSV è molto versatile e può essere utilizzato anche per scrivere dati in un file dalla scheda SD. Inoltre, è possibile usare librerie aggiuntive per manipolare i dati in modo più avanzato, come ad esempio aggiungere filtri o creare grafici.

## Vedi anche

- [Libreria SD](https://www.arduino.cc/en/Reference/SD)
- [Libreria SPI](https://www.arduino.cc/en/Reference/SPI)
- [Libreria String](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/readstringuntil/)