---
title:                "Arduino: Concatenazione di stringhe"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Perché concatenare le stringhe in Arduino

Concatenare le stringhe può essere utile in molte situazioni, soprattutto quando si vuole creare un output personalizzato o un messaggio dinamico da inviare ad un display, un'interfaccia seriale o altri dispositivi. Ci permette di combinare diverse parti di testo o variabili per creare un'unica stringa.

## Come concatenare le stringhe in Arduino

Per concatenare le stringhe in Arduino, possiamo utilizzare la funzione "concat" della libreria "ArduinoString". Di seguito un esempio di codice che combina una stringa predefinita con una variabile:

```Arduino
#include <ArduinoString.h>

// Dichiarazione della variabile
int temperatura = 25;

// Creazione di una nuova stringa che combina il testo con la variabile
ArduinoString messaggio = "La temperatura attuale è " + temperatura + "°C";

// Stampa del messaggio sulla porta seriale
Serial.println(messaggio);
```

Questo codice producirà l'output "La temperatura attuale è 25°C" sulla porta seriale.

## Approfondimento sulla concatenazione delle stringhe

La funzione "concat" della libreria "ArduinoString" permette di concatenare fino a 255 stringhe o variabili in una sola, ottenendo una maggiore flessibilità nell'output dei nostri progetti. Inoltre, possiamo anche utilizzare la funzione "strcpy" per copiare il contenuto di una stringa in un'altra.

È importante notare che quando si concatenano le stringhe, ogni stringa concatenata viene salvata in memoria, quindi è fondamentale prestare attenzione alla quantità di spazio disponibile.

# Vedi anche

- [Documentazione sulla funzione "concat" di ArduinoString](https://arduiniana.org/libraries/arduinostring)
- [Esempi di concatenazione delle stringhe su Arduino](https://www.arduino.cc/reference/it/language/variables/data-types/string/functions/concat/)
- [Guida per principianti su come utilizzare le stringhe in Arduino](https://www.electronicshub.org/arduino-string/)

Con la funzione di concatenazione delle stringhe, possiamo facilmente creare un output personalizzato e dinamico nei nostri progetti Arduino. Speriamo che questo articolo ti sia stato utile nella comprensione di questa funzionalità e ti invitiamo a esplorare ulteriormente le potenzialità delle stringhe in Arduino.