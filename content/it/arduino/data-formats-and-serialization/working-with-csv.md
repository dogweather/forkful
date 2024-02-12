---
title:                "Lavorare con i CSV"
aliases:
- /it/arduino/working-with-csv.md
date:                  2024-02-03T19:18:52.167190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con i file CSV (Comma-Separated Values) su Arduino comporta la lettura e la scrittura da e verso file CSV generalmente memorizzati su una scheda SD, consentendo la registrazione dei dati, la configurazione delle impostazioni e altro ancora. I programmatori spesso gestiscono i CSV per la raccolta dei dati dei sensori, la memorizzazione dei parametri di configurazione o l'interfacciamento con altri sistemi, grazie alla sua semplicità e alla sua ampia adozione su diverse piattaforme.

## Come fare:
Arduino non dispone di una libreria integrata specificamente per la gestione di file CSV, ma è possibile utilizzare le librerie `SD` e `SPI` per accedere ai file su una scheda SD e quindi analizzare o generare dati CSV utilizzando tecniche di manipolazione delle stringhe di base. Quando si tratta di manipolare file CSV più complessi, la libreria di terze parti `ArduinoCSV` può essere utilizzata per semplificare l'analisi e la scrittura.

**Lettura dei dati CSV da una scheda SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inizializzazione fallita!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      Serial.println(dataLine); // Stampa la linea CSV
    }
    dataFile.close();
  } else {
    Serial.println("Errore nell'apertura di data.csv");
  }
}

void loop() {
  // Non usato in questo esempio
}
```
*Risultato Esempio:*
```
SensorID, Timestamp, Value
1, 1597840923, 23.5
2, 1597840987, 22.4
```

**Scrittura di dati CSV su una scheda SD:**
```cpp
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inizializzazione fallita!");
    return;
  }
  File dataFile = SD.open("output.csv", FILE_WRITE);
  if (dataFile) {
    dataFile.println("SensorID, Timestamp, Value"); // Intestazione CSV
    dataFile.println("1, 1597840923, 23.5"); // Esempio di riga di dati
    dataFile.close();
    Serial.println("Dati scritti");
  } else {
    Serial.println("Errore nell'apertura di output.csv");
  }
}

void loop() {
  // Non usato in questo esempio
}
```
*Risultato Esempio:*
```
Dati scritti
```

**Utilizzo di ArduinoCSV per l'analisi:**
Se si devono gestire file CSV complessi, la libreria `ArduinoCSV` può semplificare notevolmente gli sforzi di analisi. Questo esempio presuppone che tu abbia già installato la libreria `ArduinoCSV`.

```cpp
#include <SPI.h>
#include <SD.h>
#include <ArduinoCSV.h>

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Inizializzazione fallita!");
    return;
  }
  File dataFile = SD.open("data.csv");
  if (dataFile) {
    CSVParser parser;
    while (dataFile.available()) {
      String dataLine = dataFile.readStringUntil('\n');
      if (parser.parseLine(dataLine)) {
        for (int i = 0; i < parser.count(); i++) {
          Serial.print(parser.getField(i)); // Stampa ogni campo
          if (i < parser.count() - 1) {
            Serial.print(", ");
          }
        }
        Serial.println();
      }
    }
    dataFile.close();
  } else {
    Serial.println("Errore nell'apertura di data.csv");
  }
}

void loop() {
  // Non usato in questo esempio
}
```
*Risultato Esempio:*
```
SensorID,  Timestamp,  Value
1,  1597840923,  23.5
2,  1597840987,  22.4
```
In questi esempi, leggendo da e scrivendo su file CSV su una scheda SD, i progetti Arduino possono facilmente raccogliere dati, memorizzare impostazioni di configurazione o scambiare dati con altre applicazioni in un formato universalmente accessibile.
