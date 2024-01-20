---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:34:36.064988-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una data da stringa significa estrarre informazioni (giorno, mese, anno) da un testo. Si fa per manipolare date in formati diversi o per interfacciarsi con sensori e servizi esterni.

## How to:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS1307 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Modulo RTC non trovato!");
    while (1);
  }

  if (!rtc.isrunning()) {
    Serial.println("RTC non attivo!");
  }

  // Imposta data e ora manualmente come esempio
  // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));

  // Esempio di parsing: converti stringa "DD/MM/YYYY hh:mm:ss" in DateTime
  String dataStr = "31/12/2023 15:30:45";
  DateTime data = convertiStringaInData(dataStr);
  Serial.println(data.timestamp()); 
}

void loop() {
}

DateTime convertiStringaInData(String str) {
  int Giorno = str.substring(0, 2).toInt();
  int Mese = str.substring(3, 5).toInt();
  int Anno = str.substring(6, 10).toInt();
  int Ora = str.substring(11, 13).toInt();
  int Minuti = str.substring(14, 16).toInt();
  int Secondi = str.substring(17, 19).toInt();

  return DateTime(Anno, Mese, Giorno, Ora, Minuti, Secondi);
}
```

## Deep Dive
Il parsing della data è sempre stato un punto dolente per via dei diversi formati usati nel mondo. Alcune librerie come `RTClib` facilitano la gestione delle date su Arduino. Se non si usa un modulo RTC, si può fare il parsing manualmente come mostrato sopra. Tuttavia, attenzione ai formati di data diversi: negli Stati Uniti si usa MM/GG/AAAA, in Italia GG/MM/AAAA.

## See Also
- `RTClib` library on GitHub: [https://github.com/adafruit/RTClib](https://github.com/adafruit/RTClib)
- Reference for the `DateTime` class: [https://github.com/adafruit/RTClib#datetime](https://github.com/adafruit/RTClib#datetime)