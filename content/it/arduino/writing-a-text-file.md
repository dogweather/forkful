---
title:                "Arduino: Scrivere un file di testo"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Perché

Scrivere un file di testo è un'attività fondamentale per i programmatori Arduino. Può essere utile per salvare dati o per leggere e scrivere informazioni su una scheda SD. In questo articolo, imparerai come scrivere un file di testo utilizzando il tuo Arduino.

# Come farlo

Iniziamo con un esempio semplice di codice per scrivere un file di testo su una scheda SD utilizzando Arduino.

```Arduino
#include <SPI.h>
#include <SD.h>

File file;

void setup() {
  pinMode(4, OUTPUT);
  // inizializza la scheda SD
  SD.begin();
  // apri il file di testo
  file = SD.open("testo.txt", FILE_WRITE);
  // scrivi all'interno del file
  file.println("Ciao, Arduino!");
  // chiudi il file
  file.close()
}

void loop() {
  // lascia vuoto
}
```

Il codice sopra utilizza la libreria SPI e SD di Arduino. Inizializziamo la scheda SD e poi apriamo il file specificando il suo nome e la modalità in cui vogliamo scriverci (in questo caso, utilizzeremo "FILE_WRITE" per aggiungere testo alla fine del file). Utilizziamo il metodo `println()` per scrivere una riga di testo all'interno del file e poi lo chiudiamo con il metodo `close()`. 

L'output di questo codice sarà un file di testo chiamato "testo.txt" sulla tua scheda SD, contenente il messaggio "Ciao, Arduino!".

Oltre a scrivere testo, possiamo anche leggere un file di testo utilizzando Arduino. Ad esempio, se vogliamo leggere le righe di testo presenti nel file "testo.txt", possiamo utilizzare questo codice:

```Arduino
#include <SPI.h>
#include <SD.h>

File file;

void setup() {
  pinMode(4, OUTPUT);
  // inizializza la scheda SD
  SD.begin();
  // apri il file di testo
  file = SD.open("testo.txt");
  // leggi il file riga per riga
  while (file.available()) {
    // leggi ogni riga e stampala sulla seriale
    Serial.println(file.readStringUntil('\n'));
  }
  // chiudi il file
  file.close();
}

void loop() {
  // lascia vuoto
}
```

In questo caso, abbiamo utilizzato il metodo `available()` per verificare se ci sono ancora righe da leggere nel file. Se è così, utilizziamo il metodo `readStringUntil()` per leggere una riga alla volta e stamparla sulla seriale.

# Approfondimento

Per un approfondimento sull'argomento, puoi esplorare ulteriori funzioni e opzioni disponibili per scrivere e leggere file di testo con Arduino. Ad esempio, puoi utilizzare la funzione `seek()` per spostarti in una posizione specifica all'interno del file o puoi utilizzare la libreria `SDFat` per avere ulteriori opzioni di lettura e scrittura.

# Vedi anche

- [Libreria SD di Arduino](https://www.arduino.cc/en/Reference/SD)
- [Guida per l'utilizzo di schede SD con Arduino](https://www.circuitar.com/how-to-use-sd-card-shield-with-arduino/)
- [Libreria SDFat per Arduino](https://github.com/greiman/SdFat)