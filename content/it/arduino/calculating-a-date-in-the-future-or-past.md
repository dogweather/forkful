---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Arduino: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Calcolare una data nel futuro o nel passato consiste nell'aggiungere o sottrarre un certo numero di giorni da una data specifica. I programmatori lo fanno per gestire operazioni legate al tempo, come i conteggi alla rovescia o la pianificazione di eventi.

## Come fare:

Di seguito è riportato un esempio di codice Arduino per calcolare una data futura. Assicuriamoci di avere la libreria `TimeLib` inclusa nel tuo sketch.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(11, 30, 0, 1, 1, 2022); // Imposta l'ora corrente
}

void loop() {
  time_t futureTime = now() + (7 * DAYS); // Aggiungi 7 giorni
  
  Serial.print(day(futureTime));
  Serial.print("/");
  Serial.print(month(futureTime));
  Serial.print("/");
  Serial.println(year(futureTime));

  delay(5000); // Attendi 5 secondi prima della prossima stampa
}
```
Questo sketch genererà l'output seguente:

```Arduino
8/1/2022
```

## Approfondimento:

Nel passato, i programmatori dovevano gestire manualmente le operazioni di calcolo del tempo, compresi i dettagli complicati come gli anni bisestili. Adesso, le librerie come `TimeLib` di Arduino semplificano molto il processo.

Un'alternativa al calcolo di una data nel futuro o nel passato è l'utilizzo di un servizio di timestamp UNIX, che usa secondi invece di giorni. Può essere più preciso, ma potrebbe richiedere più lavoro per convertire i secondi in un formato di data leggibile.

Per calcolare una data nel futuro o nel past, `TimeLib` converte innanzitutto il momento attuale in secondi, quindi aggiunge o sottrae il numero appropriato di secondi.

## È possibile consultare inoltre:

- Documentazione di Arduino su `TimeLib`: https://playground.arduino.cc/Code/time
- Conversione del tempo Unix in Arduino: https://arduinogetstarted.com/tutorials/arduino-time-unix
- Tutorial su come utilizzare la libreria `TimeLib`: https://makerguides.com/time-arduino-tutorial/