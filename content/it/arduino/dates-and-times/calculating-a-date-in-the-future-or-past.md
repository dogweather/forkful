---
title:                "Calcolo di una data futura o passata"
aliases: - /it/arduino/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:28:40.679439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Calcolare una data nel futuro o nel passato significa trovare una data specifica a partire da un'altra aggiungendo o sottraendo giorni, mesi o anni. I programmatori lo fanno per gestire eventi pianificati, scadenze o semplicemente per tracciare il tempo trascorso.

## Come fare:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Modulo RTC non trovato!");
    while (1);
  }
  if (rtc.lostPower()) {
    Serial.println("RTC ha perso l'alimentazione, impostiamo la data e l'ora!");
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  DateTime futureDate = now + TimeSpan(30,0,0,0); // aggiunge 30 giorni alla data attuale

  // Stampa la data attuale
  Serial.print("Data attuale: ");
  stampaData(now);

  // Stampa la data futura
  Serial.print("Data futura: ");
  stampaData(futureDate);

  delay(10000); // Aspetta 10 secondi tra le stampe
}

void stampaData(DateTime date) {
  Serial.print(date.day());
  Serial.print("/");
  Serial.print(date.month());
  Serial.print("/");
  Serial.print(date.year());
  Serial.println();
}
```
Output:
```
Data attuale: 1/4/2023
Data futura: 1/5/2023
```

## Approfondimento
Calcolare date future o passate è essenziale da quando i computer gestiscono appuntamenti e scadenze. Prima degli RTC (Real Time Clock) digitali, si usavano metodi più primitivi basati sui cicli di sistema. Opzioni alternative includono l'uso della libreria `TimeLib.h` o servizi esterni via internet. Si deve tenere conto di complicazioni come gli anni bisestili o i cambi di ora legati alla stagionalità, che possono essere gestiti automaticamente dalle librerie.

## Vedi anche:
- Documentazione su `RTClib`: https://github.com/adafruit/RTClib
- Libreria `Time`: https://www.arduino.cc/en/Reference/Time
- Specifiche NTP (Network Time Protocol) per sincronizzare con l'ora di internet: https://www.ntp.org/
