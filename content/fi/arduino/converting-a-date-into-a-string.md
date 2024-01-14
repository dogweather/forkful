---
title:    "Arduino: Muunna päivämäärä merkkijonoksi."
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

### Miksi
Oletko koskaan halunnut muuttaa päivämäärän merkkijonoksi Arduino-ohjelmoinnin yhteydessä? Tässä blogikirjoituksessa käymme läpi, miksi tämä voi olla hyödyllistä ja miten se tehdään.

### Miten se tehdään
Kun haluat muuttaa päivämäärän merkkijonoksi, voit käyttää `toString()` funktiota yhdessä `String` luokan kanssa. Alla on esimerkki koodi, jossa päivämäärä muutetaan merkkijonoksi ja tulostetaan sarjamonitorille:

```Arduino
#include <RTClib.h> // sisältää RTC-kirjaston
RTC_DS1307 rtc; // luo RTC-olion

void setup() {
  Serial.begin(9600); // käynnistää sarjamonitorin
  rtc.begin(); // alustaa RTC:n
}

void loop() {
  DateTime now = rtc.now(); // tallentaa nykyisen ajanmuutoksen muuttujaan
  String date = now.toString("dd/MM/yyyy"); // muuntaa päivämäärän merkkijonoksi
  Serial.println(date); // tulostaa merkkijonon sarjamonitorille
  delay(1000); // odottaa sekunnin
}
```

Koodin tulostama sarjamonitori näyttäisi tältä:
```
23/08/2021
```

### Syväsukellus
Päivämäärän muuttaminen merkkijonoksi voi olla hyödyllistä esimerkiksi silloin, kun haluat tulostaa päivämäärän helposti luettavassa muodossa tai tallentaa sen johonkin tiedostoon. `DateTime` luokassa on monia muitakin käteviä funktioita, kuten `day()`, `month()` ja `year()`, joiden avulla voit käyttää erikseen päivää, kuukautta ja vuotta, jos haluat muokata päivämäärää enemmän.

### Katso myös
- [RTC-kirjasto](https://www.arduino.cc/reference/en/libraries/rtclib/)
- [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [RTC DS1307 moduuli](https://www.arduino.cc/en/Main/ArduinoBoardRTCDS1307)