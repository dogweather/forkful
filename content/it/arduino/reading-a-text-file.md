---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Cos'è e Perché?

Leggere un file di testo vuol dire estrarre ed utilizzare informazioni memorizzate come dati testuali in un file. I programmatori fanno questa operazione per accedere a dati memorizzati esternamente all'applicazione.

# Come si fa:

Ecco un esempio di codice Arduino per leggere un file di testo.

```Arduino
#include <SD.h>

File mioFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin(4)) {
    Serial.println("Impossibile inizializzare la SD");
    return;
  }

  mioFile = SD.open("testo.txt");
  if (mioFile) {
    while (mioFile.available()) {
      Serial.write(mioFile.read());
    }
    mioFile.close();
  } else {
    Serial.println("Errore di apertura del file");
  }
}

void loop() {
  // non fare nulla
}
```
Questo codice inizializza la scheda SD, apre un file di testo e lo legge riga per riga, stampando il contenuto sul monitor seriale.

# In Profondità

Storicamente, la lettura di file di testo è uno dei primi metodi utilizzati per lo storage dei dati. Pur essendo un concetto semplice, è ancora ampiamente utilizzato oggi per la sua praticità ed accessibilità.

Le alternative alla lettura di file di testo includono l'uso di database o l'accesso a servizi web per recuperare i dati. Queste opzioni possono offrire maggiore velocità o funzionalità extra, ma spesso richiedono più risorse e possono essere overkill per progetti semplici.

Per quanto riguarda l'implementazione dettagliata di lettura di un file di testo con Arduino, il "File" è un oggetto che agisce come un link al file su SD. Il metodo "available()" restituisce il numero di byte disponibili per la lettura, e il metodo "read()" legge il prossimo byte disponibile dal file.

#Vedi Ancora

1. Documentazione ufficiale Arduino su SD library: [https://www.arduino.cc/en/Reference/SD](https://www.arduino.cc/en/Reference/SD)
2. Tutorial Fritzing su lettura di file di testo con Arduino: [http://fritzing.org](http://fritzing.org)
3. Post su Arduino StackExchange sulla lettura di file di testo: [https://arduino.stackexchange.com](https://arduino.stackexchange.com)