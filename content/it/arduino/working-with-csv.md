---
title:                "Lavorare con i file CSV"
date:                  2024-01-19
simple_title:         "Lavorare con i file CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Lavorare con i file CSV significa manipolare dati in formato "Comma-Separated Values" (Valori Separati da Virgola), strumento semplice per gestire dati tabellari. I programmatori li usano per la loro leggibilità e la facilità di importazione/esportazione da database e fogli di calcolo.

## Come Fare:
```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(10); // Assicurati che il pin chip select sia corretto per il tuo modulo

  myFile = SD.open("datalog.csv", FILE_WRITE);

  // Se il file è aperto correttamente, scrivi su di esso
  if (myFile) {
    myFile.println("id,temperature,humidity");
    myFile.println("1,22.5,45");
    myFile.println("2,23.1,47");
    // Chiudi il file
    myFile.close();
  } else {
    // se il file non si apre, avvisa su seriale
    Serial.println("Errore nell'apertura del file");
  }
}

void loop() {
  // Vuoto. La logica di scrittura è tutta in setup.
}
```
Nell'esempio, un Arduino crea un file CSV con dati fittizi.

## Approfondimento
Il formato CSV fu sviluppato nei primi anni dell'informatica per scambiare dati tra programmi incompatibili. Mentre ci sono alternative come JSON o XML, CSV resta un pilastro per la sua semplicità ed efficienza in contesti dove la struttura complessa non è necessaria. Nell'implementazione, prestare attenzione alla formattazione corretta e all'escape di caratteri speciali (es., virgole nei dati).

## Vedi Anche
- La [documentazione ufficiale della libreria SD di Arduino](https://www.arduino.cc/en/Reference/SD)
- [Tutorial sull'utilizzo di file CSV con Python](https://realpython.com/python-csv/)
- [Specifiche formali del formato CSV da RFC 4180](https://tools.ietf.org/html/rfc4180)
