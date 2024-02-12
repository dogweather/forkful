---
title:                "Verifica se una directory esiste"
aliases:
- /it/arduino/checking-if-a-directory-exists/
date:                  2024-02-03T19:06:39.648708-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verifica se una directory esiste"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Nel contesto della programmazione Arduino, verificare se una directory esiste su una scheda SD o su un modulo di memoria simile consente di leggere o scrivere file senza errori. Questa operazione è essenziale per il registro dati, la gestione della configurazione o qualsiasi compito che richieda una memorizzazione dei file strutturata, garantendo affidabilità e prestazioni fluide nelle tue applicazioni.

## Come fare:
Arduino di base non supporta operazioni complesse sui file system direttamente. Tuttavia, con l'utilizzo della libreria SD, che fa parte dell'IDE standard di Arduino, puoi facilmente lavorare con file e directory. Per verificare se una directory esiste, devi prima inizializzare la scheda SD e poi utilizzare il metodo `exists()` dalla libreria SD.

Prima, include la libreria SD e dichiara il pin di selezione del chip:

```cpp
#include <SPI.h>
#include <SD.h>

const int chipSelect = 4; // Pin di selezione del chip per il modulo della scheda SD
```

Nella tua funzione `setup()`, inizializza la scheda SD e controlla se la directory esiste:

```cpp
void setup() {
  Serial.begin(9600);
  
  if (!SD.begin(chipSelect)) {
    Serial.println("Inizializzazione fallita!");
    return;
  }

  // Controlla se la directory esiste
  if (SD.exists("/myDir")) {
    Serial.println("La directory esiste.");
  } else {
    Serial.println("La directory non esiste.");
  }
}
```
Nella funzione `loop()`, puoi lasciarla vuota o aggiungere altri codici operativi come richiesto:

```cpp
void loop() {
  // Codice operativo o tenere vuoto
}
```

L'output di esempio all'esecuzione del codice sarà:

```
La directory esiste.
```
oppure

```
La directory non esiste.
```

È importante assicurarsi che la scheda SD sia formattata correttamente e che il percorso della directory `/myDir` sia in linea con le tue specifiche esigenze. Questo controllo di base è una pietra angolare per eseguire operazioni più complesse con file e directory su schede SD con Arduino.
