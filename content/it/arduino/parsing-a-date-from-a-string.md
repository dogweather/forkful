---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
L'analisi di una data da una stringa è il processo di trasformazione di un testo in un formato di data riconosciuto dal tuo programma. I programmatori lo fanno per gestire e manipolare le date in modi più utili e efficienti.

## Come fare:
Ecco un esempio semplice di parsing di una data da una stringa in Arduino. Questo codice riconoscerà una data in formato "giorno/mese/anno".

```Arduino
#include <TimeLib.h>

void setup(){
  Serial.begin(9600);
}

void loop(){
  String dataString = "21/12/2021";
  // Disgregare la stringa in giorno, mese ed anno
  int giorno = dataString.substring(0,2).toInt();
  int mese = dataString.substring(3,5).toInt();
  int anno = dataString.substring(6,10).toInt();
  // Impostare la data
  setTime(ora()/3600, minuto()/60, secondo()%60, giorno, mese, anno);
  // Visualizzare la data
  Serial.println(monthShortStr(month()));
  delay(1000);
}
```
Eseguendo il programma, otterremo in output il mese corrispondente, quindi "DEC" nel caso dell'esempio precedente.

## Approfondimento
(1) Storicamente, il parsing di date da stringhe è sempre stato un punto delicato nella programmazione, poiché le date possono essere rappresentate in molti formati diversi.
(2) Ci sono molte alternative per il parsing di una data da una stringa. Ad esempio, si possono utilizzare le librerie esterne come la "DateStrings" che offre molte funzioni utili.
(3) Tuttavia, nel nostro esempio abbiamo deciso di rimanere con il core di Arduino e la libreria TimeLib, che fornisce un semplice accessorio per manipolare i dati di tempo di sistema.

## Guarda Anche
Per approfondire ulteriormente l'argomento, ecco alcuni link utili:

- Documentazione Arduino TimeLib: https://www.arduino.cc/reference/en/libraries/timelib/
- Un ottima guida a "DateStrings library": https://www.arduino.cc/en/Tutorial/LibraryExamples/Time
- Libreria Time di Paul Stoffregen: https://github.com/PaulStoffregen/Time
- Un'introduzione generale alla gestione del tempo in Arduino: https://learn.adafruit.com/adafruit-ultimate-gps/logger-shield-time-management