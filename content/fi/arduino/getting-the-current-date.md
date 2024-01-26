---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:13:08.500787-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
"Mikä & Miksi?"

Ajan päivittäinen noutaminen kertoo, millä hetkellä ollaan menossa. Sitä käytetään tapahtumien aikaleimojen luomiseen, aikariippuvaisten toimintojen ohjaamiseen ja käyttäjien informoimiseen päivämäärästä.

## How to:
"Miten tehdä:"

```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Ei löydetä RTC:ta");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC on nollattu, aseta aika!");
    // Näin asetetaan aika: rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  
  Serial.print("Päiväys: ");
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.print(now.year());
  Serial.print(" Aika: ");
  Serial.print(now.hour());
  Serial.print(':');
  Serial.print(now.minute());
  Serial.print(':');
  Serial.println(now.second());

  delay(1000);
}
```
Saatu tuloste näyttää tältä:  
`Päiväys: 15/4/2023 Aika: 12:34:56`

## Deep Dive:
"Syväsukellus"

Arduino ei sisällä sisäänrakennettua reaaliaikakelloa (RTC). Tämän vuoksi käytetään ulkoista RTC-moduulia, kuten DS3231. RTC-moduulit ylläpitävät aikaa paristolla, joten ne toimivat vaikka Arduino sammutettaisiin.

Historiallisesti ajannouginta tapahtui eri tavoilla, kuten loopissa olevilla viiveillä, mutta nämä menetelmät eivät olleet tarkkoja. RTC-moduulit tarjoavat tarkan ja luotettavan tavan seurata aikaa.

Käyttäjät voivat käyttää muita kirjastoja tai moduuleja, kuten Time.h tai GPS-moduulit saadakseen ajan. Käytettävä menetelmä riippuu projektista ja tarvittavasta tarkkuudesta.

## See Also:
"Katso myös"

- RTClib-kirjaston dokumentaatio: https://github.com/adafruit/RTClib
- Arduino Time-kirjasto: https://www.arduino.cc/en/Reference/Time
- DS3231 moduulin datalehti: https://datasheets.maximintegrated.com/en/ds/DS3231.pdf
