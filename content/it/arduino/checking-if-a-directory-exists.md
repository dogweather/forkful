---
title:                "Arduino: Verificare l'esistenza di una directory"
simple_title:         "Verificare l'esistenza di una directory"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore di Arduino, probabilmente hai bisogno di sapere se una directory esiste o meno per gestire i file su una scheda di memoria. In questo articolo, ti mostrerò come verificare l'esistenza di una directory utilizzando il tuo codice Arduino.

## Come verificare l'esistenza di una directory in Arduino

Per prima cosa, devi includere la libreria `SD.h` nel tuo codice.

```
#include <SD.h>
```

Successivamente, devi inizializzare la connessione alla scheda di memoria con il seguente codice:

```
if (!SD.begin(chipSelect)) {
  Serial.println("Errore di inizializzazione!");
  return;
}
```

Assicurati di sostituire "chipSelect" con il numero della tua pin CS (Chip Select).

Una volta che sei connesso alla scheda di memoria, puoi verificare l'esistenza di una directory con la funzione `SD.exists()` utilizzando il seguente codice:

```
if (SD.exists("/miodirectory")) {
  Serial.println("La directory esiste!");
} else {
  Serial.println("La directory non esiste!");
}
```

Questa funzione restituirà un valore booleano, "true" se la directory esiste e "false" se non esiste.

## Approfondimento

Puoi anche utilizzare la funzione `SD.mkdir()` per creare una nuova directory nel tuo codice Arduino. Tuttavia, nota che assumerà il nome della directory specificato solo se non esiste già una directory con lo stesso nome.

È anche possibile utilizzare la funzione `SD.rmdir()` per rimuovere una directory esistente dal tuo codice.

## Vedi anche

- [Documentation for SD library](https://www.arduino.cc/en/Reference/SD)
- [Arduino SD library tutorial](https://randomnerdtutorials.com/guide-to-sd-card-module-with-arduino/)