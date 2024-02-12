---
title:                "Nykyisen päivämäärän hankkiminen"
aliases:
- /fi/arduino/getting-the-current-date/
date:                  2024-02-03T19:09:00.393780-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Nykyisen päivämäärän saaminen Arduino-projekteissa tarkoittaa reaaliaikaisen tiedon hankkimista, joka voi olla ratkaisevan tärkeää lokitiedostojen kirjaamisessa, aikaleimojen lisäämisessä tai tehtävien ajoittamisessa. Ohjelmoijat tarvitsevat usein tätä kykyä toiminnallisuuden parantamiseen, datan ajantasaisuuden varmistamiseen ja aikaherkkien operaatioiden helpottamiseen IoT- ja sulautetuissa projekteissa.

## Kuinka:
Arduino itsessään ei sisällä sisäänrakennettua menetelmää nykyisen päivämäärän suoraan noutamiseen, sillä siitä puuttuu reaaliaikakello (RTC). Tämän voi kuitenkin saavuttaa käyttämällä ulkoisia RTC-moduuleja, kuten DS3231, ja kirjastoja, kuten `RTClib`, jonka on kehittänyt Adafruit, ja joka tekee näiden moduulien kanssa rajapinnan muodostamisesta suoraviivaista.

Varmista ensin, että `RTClib`-kirjasto on asennettu Arduino IDE:eesi. Kytke sitten RTC-moduulisi Arduinoon sen dokumentaation mukaisesti.

Tässä on yksinkertainen esimerkki aloittamiseen:

```cpp
#include <Wire.h>
#include "RTClib.h"

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Ei löydetty RTC:tä");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC menetti virran, asetetaan aika!");
    // Kun laitteella on uusi tai sen jälkeen kun se on menettänyt virran ja aika täytyy asettaa, voit asettaa sen tässä.
    // rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print("Nykyinen päivämäärä: ");
  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.println(now.day(), DEC);

  delay(3000); // Viivästys 3 sekuntia vähentämään sarjatulostuksen määrää
}
```

Esimerkkilähtötieto (olettaen, että RTC on aiemmin asetettu):

```
Nykyinen päivämäärä: 2023/4/15
```

Tämä koodi alustaa RTC-moduulin ja hakee sitten pääsilmukassa ja tulostaa nykyisen päivämäärän sarjatarkkailuun joka 3. sekunti. Muista, että `rtc.adjust(...)`-rivi voidaan ottaa kommentista pois ja muuttaa, jotta RTC:n päivämäärä ja aika voidaan asettaa alunperin tai sen jälkeen, kun se on menettänyt virran.
