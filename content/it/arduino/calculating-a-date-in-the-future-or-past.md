---
title:                "Arduino: Calcolare una data nel futuro o passato"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare le date nel futuro o nel passato può essere utile per diversi progetti Arduino, come ad esempio un calendario elettronico o un orologio digitale. Può anche essere utile per creare un sistema di notifiche basato sulla data.

## Come fare
Per calcolare una data futura o passata con Arduino, è necessario utilizzare la libreria "Time". Inoltre, è necessario impostare l'orario corrente utilizzando un modulo RTC (Real Time Clock) o una connessione Internet. Una volta impostato l'orario corrente, è possibile utilizzare le funzioni della libreria "Time" per calcolare la data desiderata.

Ecco un esempio di codice Arduino per calcolare una data nel futuro usando la libreria "Time":

```Arduino
#include <TimeLib.h>
#include <Time.h>

// Imposta l'orario corrente
const int giorno = 30;
const int mese = 6;
const int anno = 2021;
const int ora = 12;
const int minuti = 0;
const int secondi = 0;

void setup(){
  // Imposta l'orario corrente
  setTime(ora, minuti, secondi, giorno, mese, anno);
}

void loop(){
  // Calcola la data in futuro
  time_t future = now() + 86400; // Aggiunge un giorno in secondi

  // Stampa la data
  Serial.print(day(future));
  Serial.print("/");
  Serial.print(month(future));
  Serial.print("/");
  Serial.print(year(future));

  delay(1000); // attende un secondo prima di calcolare di nuovo la data
}
```

Ecco l'output che verrà visualizzato nella console seriale:

`1/7/2021`

## Approfondimento
Calcolare una data nel futuro o nel passato può sembrare semplice, ma ci sono alcuni fattori da tenere in considerazione, come ad esempio gli anni bisestili e il cambio dell'ora durante l'orario legale. La libreria "Time" tiene conto di questi fattori e assicura che la data calcolata sia sempre corretta.

Inoltre, è possibile utilizzare le funzioni della libreria "Time" per calcolare non solo la data, ma anche l'orario nel futuro o nel passato.

## Vedi anche
- Documentazione ufficiale della libreria "Time": https://www.arduino.cc/en/reference/time
- Tutorial su come utilizzare una libreria di orologio in tempo reale con Arduino: https://create.arduino.cc/projecthub/madartsystm/using-an-rtc-in-arduino-project-da3f05
- Video tutorial su come calcolare una data futura o passata con Arduino: https://www.youtube.com/watch?v=ZN6nHYxI4LM