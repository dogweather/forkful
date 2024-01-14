---
title:                "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo in Arduino può essere utile per salvare dati o informazioni importanti. Ad esempio, un sensore potrebbe raccogliere dati da una pianta e scrivere un file di testo per tenere traccia di quando è stato innaffiato, quanto è stato innaffiato e se sono state utilizzate sostanze nutritive.

## Come fare

Per scrivere un file di testo in Arduino, seguire questi semplici passaggi:

1. Dichiarare una variabile di tipo `File` e assegnarle un nome, ad esempio `data`.
2. Utilizzare la funzione `data.write()` per scrivere i dati all'interno del file.
3. Utilizzare la funzione `data.close()` per chiudere il file e salvare le modifiche.

Ecco un esempio di codice:

```Arduino
#include <SD.h> //include la libreria SD
File data; //dichiara la variabile per il file

void setup() {
  //inizializza la comunicazione con la scheda SD
  if(!SD.begin(4)) {
    //stampa un messaggio di errore se la scheda SD non viene rilevata
    Serial.println("Errore nella lettura della scheda SD");
    return;
  }
}

void loop() {
  //apre il file in modalità "scrittura"
  data = SD.open("dati.txt", FILE_WRITE);
  
  //scrive dei dati all'interno del file
  data.println("Ultima innaffiatura: 01/01/2021");
  data.println("Quantità di acqua utilizzata: 50ml");
  
  //chiude il file e salva le modifiche
  data.close();
}
```

L'esempio di codice sopra aprirà un file di testo chiamato "dati.txt" sulla scheda SD e scriverà due righe di dati all'interno del file. È importante notare che il pin 4 è stato utilizzato per comunicare con la scheda SD, ma questo può variare a seconda del modello di Arduino che si sta utilizzando.

## Approfondimento

Per accedere ai dati all'interno del file di testo, è possibile utilizzare la funzione `data.read()` o `data.readString()` per leggere il contenuto del file. Inoltre, è possibile utilizzare la funzione `data.available()` per verificare se ci sono dati disponibili all'interno del file.

È anche importante notare che è possibile creare cartelle all'interno della scheda SD utilizzando la funzione `SD.mkdir()`, in modo da organizzare i file in modo più efficiente.

Inoltre, nel caso in cui non si voglia scrivere un file di testo sulla scheda SD, è possibile utilizzare la memoria interna di Arduino utilizzando la funzione `EEPROM.write()` e `EEPROM.read()`.

## Vedi anche

- Scheda SD Arduino: https://www.arduino.cc/en/Reference/SD
- Tutorial su come scrivere file di testo: https://www.arduino.cc/en/Tutorial/LibraryExamples/WriteToFile
- Manipolazione della memoria di Arduino: https://www.arduino.cc/en/Reference/EEPROM