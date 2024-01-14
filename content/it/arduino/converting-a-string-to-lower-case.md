---
title:                "Arduino: Conversione di una stringa in minuscolo"
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Spesso quando scriviamo codice su Arduino, dobbiamo gestire stringhe di testo. In alcune situazioni, potrebbe essere necessario convertire il testo in lettere minuscole per facilitare la manipolazione dei dati. In questo articolo, vedremo come convertire una stringa in lettere minuscole utilizzando il linguaggio di programmazione di Arduino.

## Come fare

Per convertire una stringa in lettere minuscole su Arduino, abbiamo bisogno di utilizzare una funzione chiamata `toLowerCase()`. Questa funzione è disponibile nella libreria standard Arduino String.h e viene utilizzata per convertire una stringa in lettere minuscole.

Ecco un esempio di codice che mostra come utilizzare la funzione `toLowerCase()`:

```Arduino
#include <String.h>

void setup() {
  Serial.begin(9600);
  
  String str = "QUESTA È UNA STRINGA IN MAIUSCOLO";
  
  Serial.println("Stringa originale: " + str);
  Serial.println("Stringa convertita in minuscolo: " + str.toLowerCase());
}

void loop() {
  
}
```

In questo esempio, abbiamo dichiarato una variabile `String` chiamata `str` con una stringa in maiuscolo come valore. Successivamente, tramite la funzione `toLowerCase()`, abbiamo convertito la stringa in lettere minuscole e stampato il risultato utilizzando `Serial.println()`.

L'output di questo codice sarà il seguente:

```Arduino
Stringa originale: QUESTA È UNA STRINGA IN MAIUSCOLO
Stringa convertita in minuscolo: questa è una stringa in maiuscolo
```

Come puoi vedere, la funzione `toLowerCase()` ha convertito correttamente la stringa in lettere minuscole.

## Approfondimento

Oltre alla funzione `toLowerCase()`, ci sono anche altre opzioni per convertire una stringa in lettere minuscole su Arduino. Ad esempio, puoi utilizzare la funzione `toCharArray()` per convertire la stringa in un array di caratteri e successivamente convertire ogni carattere in lettera minuscola utilizzando la funzione `tolower()`. Questo metodo potrebbe essere utile se devi manipolare ogni carattere individualmente.

Inoltre, è importante notare che la funzione `toLowerCase()` è sensibile alla lingua e alle impostazioni regionali del tuo dispositivo Arduino. Quindi, se hai bisogno di una conversione accurata per una specifica lingua, potresti dover utilizzare un approccio diverso.

## Vedi anche

- [Documentazione ufficiale di Arduino String.h](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Funzione toLowerCase() - Tutorialspoint](https://www.tutorialspoint.com/arduino/arduino_string_tolowercase.htm)
- [Funzione toCharArray() - Arduino Project Hub](https://create.arduino.cc/projecthub/Nassir/toconvert-a-string-into-char-array-4a0c67)