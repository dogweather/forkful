---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:50.001808-07:00
description: "Ottenere la data corrente nei progetti Arduino coinvolge l'acquisizione\
  \ di informazioni in tempo reale che possono essere cruciali per la registrazione,\u2026"
lastmod: 2024-02-19 22:05:02.769806
model: gpt-4-0125-preview
summary: "Ottenere la data corrente nei progetti Arduino coinvolge l'acquisizione\
  \ di informazioni in tempo reale che possono essere cruciali per la registrazione,\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente nei progetti Arduino coinvolge l'acquisizione di informazioni in tempo reale che possono essere cruciali per la registrazione, l'apposizione di timestamp o la programmazione di attività. I programmatori spesso necessitano di questa capacità per migliorare la funzionalità, garantire la rilevanza dei dati e facilitare le operazioni sensibili al tempo nei loro progetti IoT e embedded.

## Come fare:
Di per sé, Arduino non dispone di un metodo integrato per recuperare direttamente la data corrente, in quanto manca di un orologio in tempo reale (RTC). Tuttavia, ciò può essere ottenuto utilizzando moduli RTC esterni come il DS3231 e librerie come `RTClib`, sviluppata da Adafruit, che rende semplice l'interfacciamento con questi moduli.

Prima, assicurati che la libreria `RTClib` sia installata nel tuo Arduino IDE. Poi, collega il tuo modulo RTC al tuo Arduino secondo la sua documentazione.

Ecco un semplice esempio per iniziare:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Non trovo l'RTC");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC ha perso alimentazione, impostiamo l'ora!");
    // Quando è necessario impostare l'ora su un nuovo dispositivo o dopo una perdita di alimentazione, puoi farlo qui.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Data Corrente: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Ritardo di 3 secondi per ridurre lo spam seriale
}
```

Esempio di output (assumendo che il tuo RTC sia stato precedentemente impostato):

```
Data Corrente: 2023/4/15
```

Questo codice inizializza il modulo RTC e poi, nel loop, recupera e stampa la data corrente sul Monitor Seriale ogni 3 secondi. Ricorda, la linea `rtc.adjust(...)` può essere decommentata e modificata per impostare inizialmente o dopo una perdita di alimentazione la data e l'ora dell'RTC.
