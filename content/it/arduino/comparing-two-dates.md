---
title:                "Confronto tra due date"
html_title:           "Arduino: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Confrontare due date è un'operazione comune nella programmazione, spesso utilizzata per confrontare eventi o per calcolare la differenza di tempo tra due istanti. I programmatori svolgono questa operazione per ottenere informazioni utili e prendere decisioni basate sui dati.

## Come fare:

```
ArduinoDate data1 = ArduinoDate(7, 9, 2021); // primo istante
ArduinoDate data2 = ArduinoDate(25, 6, 2021); // secondo istante

int differenza = data1.deltaDays(data2); // calcola la differenza di giorni tra le due date

if (differenza > 0) { // se la differenza è positiva, data1 è successiva a data2
  Serial.println("La prima data è successiva alla seconda");
} else if (differenza < 0) { // se la differenza è negativa, data2 è successiva a data1
  Serial.println("La seconda data è successiva alla prima");
} else { // se la differenza è zero, le date sono uguali
  Serial.println("Le due date sono uguali");
}

```

Esempio di output: `La prima data è successiva alla seconda`

## Approfondimento:

Per confrontare due date, è possibile utilizzare diverse librerie o funzioni già disponibili per Arduino. Una di queste è la libreria `TimeLib.h`, che fornisce una serie di funzioni per manipolare date e orari. Un'altra opzione è utilizzare la classe `Calendar` presente nella libreria `RTClib.h` per gestire date e orari utilizzando un modulo RTC (Real Time Clock).

## Vedi anche:

- [Libreria TimeLib.h](https://www.arduino.cc/reference/en/libraries/timelib/)
- [Libreria RTClib.h](https://github.com/adafruit/RTClib)