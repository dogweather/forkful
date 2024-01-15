---
title:                "Lavorare con i file csv"
html_title:           "Arduino: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Csv (Comma-separated values) è un formato di file molto comune per memorizzare dati tabellari. Per chi lavora con l'Arduino, essere in grado di leggere e scrivere file CSV può essere molto utile per gestire grandi quantità di dati.

## Come fare
Per leggere un file CSV con Arduino, è necessario utilizzare la libreria "SD.h" per accedere alla scheda SD e la libreria "StringSplitter.h" per analizzare il contenuto del file.

```Arduino
#include <SD.h>
#include <StringSplitter.h>

void setup() {
  Serial.begin(9600);
  File dataFile = SD.open("file.csv"); // Apri il file CSV
  while (dataFile.available()) { // Loop finché ci sono dati disponibili
    String line = dataFile.readStringUntil('\n'); // Leggi una riga
    StringSplitter splitter(line, ','); // Divide la riga in base alla virgola
    while (splitter.hasNext()) { // Loop finché ci sono elementi
      String value = splitter.next(); // Leggi il valore
      Serial.print(value); // Stampa il valore
      Serial.print(" "); // Aggiungi uno spazio
    }
    Serial.println(); // Vai alla prossima riga
  }
  dataFile.close(); // Chiudi il file
}

void loop() {
  // Il codice del loop non è necessario per leggere un file CSV
}
```

L'output verrà stampato nel monitor seriale, con ogni valore separato da uno spazio.

## Approfondimento
Oltre alla lettura dei file CSV, è possibile anche scrivere su di essi utilizzando la libreria "SD.h". Inoltre, è possibile utilizzare la funzione "parseInt()" per convertire i valori di tipo stringa in numeri interi e la funzione "parseFloat()" per convertire i valori in numeri decimali.

## Vedi anche
- [Documentazione ufficiale di Arduino](https://www.arduino.cc/reference/en/libraries/sd/)
- [Tutorial di Adafruit](https://learn.adafruit.com/adafruit-micro-sd-breakout-board-card-tutorial/arduino-library)