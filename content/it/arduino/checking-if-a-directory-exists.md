---
title:    "Arduino: Verifica dell'esistenza di una cartella"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perchè
Se stai realizzando un progetto con Arduino che richiede di utilizzare una memoria esterna come una scheda SD, potrebbe essere utile sapere se una determinata cartella esiste o meno. In questo modo, puoi evitare errori nel tuo codice e garantire che il programma funzioni correttamente.

## Come fare
Per verificare se una cartella esiste su una scheda SD collegata ad Arduino, puoi utilizzare la funzione `exists()` della libreria `SD`. Ecco un esempio di codice:

```Arduino
#include <SD.h>

void setup() {
  // Inizializza la scheda SD
  SD.begin();

  // Verifica se la cartella "log" esiste
  if (SD.exists("log")) {
    // La cartella esiste, stampa un messaggio
    Serial.println("La cartella log esiste!");
  } else {
    // La cartella non esiste, stampa un messaggio
    Serial.println("La cartella log non esiste!");
  }
}

void loop() {
  // Il programma continua qui
}
```

Se la cartella "log" esiste, il programma stampa "La cartella log esiste!". Altrimenti, verrà stampato "La cartella log non esiste!".
Puoi anche utilizzare questa funzione all'interno di un loop per aggiungere ulteriori azioni in base alla presenza o assenza della cartella.

## Approfondimenti
E' importante notare che la funzione `exists()` non verifica se una cartella è vuota o meno, ma solo se la cartella stessa esiste.
Inoltre, se stai utilizzando una scheda SD con una capacità inferiore a 2GB, potresti riscontrare problemi con la funzione `exists()`. In questo caso, puoi utilizzare la funzione `SD.ls()` per ottenere una lista delle cartelle presenti sulla scheda e verificare la presenza della cartella desiderata all'interno della lista.

## Vedi anche
- [Documentazione di Arduino sulla libreria SD](https://www.arduino.cc/en/Reference/SD)
- [Esempi di codice per l'utilizzo di una scheda SD con Arduino](https://www.arduino.cc/en/Tutorial/ReadASCIIString)
- [Tutorial su come utilizzare una scheda SD con Arduino](https://www.circuitsdiy.com/how-to-use-sd-card-with-arduino/)