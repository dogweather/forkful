---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:12:52.139468-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Ottenere la data corrente permette ai tuoi progetti di sapere "quando". È utile per tracciare eventi, loggare dati o creare timestamp.

## How to: (Come fare:)
Prima, connetti un modulo RTC (Real Time Clock) come il DS3231 al tuo Arduino. Poi, usa questa libreria per leggere la data.

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  
  if (!rtc.begin()) {
    Serial.println("Impossibile trovare RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC perso alimentazione, imposta la data e ora!");
    // Impostazione manuale della data e dell'ora
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  Serial.print(" ");
  Serial.print(now.hour(), DEC);
  Serial.print(':');
  Serial.print(now.minute(), DEC);
  Serial.print(':');
  Serial.print(now.second(), DEC);
  Serial.println();
  
  delay(1000);
}
```
Output Campione:
```
2023/3/30 15:46:10
```

## Deep Dive (Approfondimento)
Historically, keeping time on a microcontroller non era facile: no internal clock or battery. RTC modules hanno cambiato il gioco.

Esistono alternative all'RTC, come NTP (Network Time Protocol) per dispositivi connessi o aggiornamenti orari tramite GPS.

Dettagli implementativi: RTC lib gestisce la comunicazione tramite I2C, mantenendo il codice semplice per l'utente finale.

## See Also (Vedi Anche)
- [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
- [DS3231 RTC Library](https://github.com/adafruit/RTClib)
