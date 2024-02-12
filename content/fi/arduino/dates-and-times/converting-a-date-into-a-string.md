---
title:                "Päivämäärän muuntaminen merkkijonoksi"
aliases: - /fi/arduino/converting-a-date-into-a-string.md
date:                  2024-01-20T17:35:46.940612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Muuntaminen päivämäärästä merkkijonoksi on tapa esittää päivämäärä helposti luettavassa formaatissa. Ohjelmoijat tekevät tämän, jotta voidaan tallentaa, näyttää käyttäjälle tai lähettää päivämäärätietoja.

## How to:
```Arduino
#include <RTClib.h>
RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("RTC not found!");
    while (1);
  }
  if (rtc.lostPower()) {
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();
  char buffer[20];
  sprintf(buffer, "%02d.%02d.%04d %02d:%02d:%02d", now.day(), now.month(), now.year(), now.hour(), now.minute(), now.second());
  Serial.println(buffer);
  delay(1000);
}
```
Tulostus:
```
20.04.2023 14:10:32
```

## Deep Dive
Päivämäärän merkkijonomuotoon muuntaminen on ollut tarpeellista tietokoneohjelmoinnin varhaisista päivistä. RTCLib on nykyaikainen kirjasto päivämäärän käsittelyyn Arduino-ympäristössä. Vaihtoehdot sisältävät `strftime` ja manuaalisen muodostamisen käyttäen `String` luokkaa. Esimerkkimme käyttää `sprintf`-funktiota muotoiluun, joka on tehokas, mutta vaatii tarkan bufferin koon määrittelyn.

## See Also
- RTClib-kirjaston GitHub-sivu: https://github.com/adafruit/RTClib
- Arduino DateTime-kirjaston dokumentaatio: https://www.arduino.cc/reference/en/libraries/datetime/
- C++ `strftime`-funktio: http://www.cplusplus.com/reference/ctime/strftime/
