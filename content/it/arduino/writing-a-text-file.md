---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

Arduino è una scheda di sviluppo open-source molto popolare tra gli appassionati di elettronica e programmazione. Una delle sue funzionalità interessanti è la possibilità di scrivere file di testo, che possono essere utilizzati per salvare dati o leggere informazioni da altri dispositivi.

## Perché

Scrivere un file di testo con Arduino può essere utile in molteplici situazioni. Ad esempio, si può salvare un valore di temperatura rilevato da un sensore, in modo da poterlo utilizzare in futuro per analisi o per attivare un dispositivo di raffreddamento. Inoltre, i file di testo possono essere letti da altri dispositivi, fornendo un modo semplice per scambiare informazioni tra più dispositivi.

## Come fare

Per scrivere un file di testo con Arduino, è necessario seguire questi passaggi:

1. Dichiarare un oggetto `File` utilizzando la libreria `SD` per la gestione della scheda SD.

2. Inizializzare la comunicazione con la scheda SD utilizzando il metodo `begin()` della libreria `SD`.

3. Aprire il file in modalità scrittura utilizzando il metodo `open()` dell'oggetto `File`.

4. Scrivere il contenuto da salvare all'interno del file utilizzando il metodo `write()` dell'oggetto `File`.

5. Chiudere il file utilizzando il metodo `close()` dell'oggetto `File`.

Esempio di codice:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  // Inizializza la comunicazione con la scheda SD
  SD.begin(4);

  // Apre il file "dati.txt" in modalità scrittura
  myFile = SD.open("dati.txt", FILE_WRITE);
  
  // Scrive il valore di temperatura all'interno del file
  float temperatura = 25.5;
  myFile.write(temperatura);
  
  // Chiude il file
  myFile.close();
}

void loop() {
  // Il programma continuerà a eseguire altre operazioni
}
```

## Approfondimento

È possibile modificare la modalità di apertura del file utilizzando la variabile `FileOpenMode`. Ad esempio, è possibile specificare se il file deve essere aperto in modalità scrittura, append o lettura. Inoltre, è possibile utilizzare il metodo `println()` anziché `write()` per scrivere una riga di testo seguita da un carattere di nuova riga.

Per maggiori informazioni sulla gestione dei file con Arduino, è possibile consultare la documentazione ufficiale della libreria [SD](https://www.arduino.cc/en/Reference/SD).

## Vedi anche

- [Lettura di un file di testo con Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)

- [Utilizzo della scheda SD con Arduino](https://www.arduino.cc/en/Tutorial/SdFat)

- [Gestione dei file con la libreria SD](https://www.arduino.cc/en/Reference/SDFile)