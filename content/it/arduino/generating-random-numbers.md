---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile in molte situazioni, come ad esempio nei giochi e nei sistemi di sicurezza. Usare Arduino per generare questi numeri può essere divertente e istruttivo.

## Come Fare

Per generare numeri casuali con Arduino, si può utilizzare la funzione `random(min, max)`, che restituisce un numero intero compreso tra il valore minimo e massimo specificato.

```Arduino
#include <Arduino.h>

void setup() {
  Serial.begin(9600); // inizializza la comunicazione seriale
}

void loop() {
  int random_number = random(1, 50); // genera un numero casuale compreso tra 1 e 50
  Serial.println(random_number); // stampa il numero sulla porta seriale
  delay(1000); // attende 1 secondo
}
```

L'esempio di codice sopra genera un numero casuale ogni secondo e lo stampa sulla porta seriale. Si può modificare il valore massimo o minimo passato alla funzione `random()` per cambiare il range dei numeri generati.

## Approfondimento

La funzione `random()` utilizza un algoritmo di generazione pseudo-casuale basato sull'orologio interno di Arduino. Ciò significa che i numeri generati saranno sempre gli stessi in ogni esecuzione del programma, a meno che non si cambi la sequenza dell'algoritmo.

Per ottenere numeri più casuali, si può utilizzare un generatore esterno, come un sensore di luce, rumore o di movimento. Il valore letto dal sensore può essere usato come argomento per la funzione `random()` o per modificare la sequenza dell'algoritmo di generazione.

## Vedi Anche

- Documentazione ufficiale di Arduino sulla funzione `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Tutorial su come utilizzare un sensore di luce con Arduino: https://www.arduino.cc/en/Tutorial/BuiltInExamples/AnalogInOutSerial
- Esempio di progetto utilizzando un sensore di movimento per generare numeri casuali: https://create.arduino.cc/projecthub/iot_lover/arduino-turns-into-a-random-number-generator-1e0373