---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Go: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän muuntaminen merkkijonoksi tarkoittaa, että muutat päivämäärän esitysmuodon – esim. 02.11.2020 – sellaiseksi merkkijonoksi, jota voidaan käyttää tekstissä tai tulostaa. Koodaajat tekevät näin, koska ohjelmat käsittelevät päivämääriä paremmin merkkijonoina.

## Näin se tehdään:

```Arduino 
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  setTime(10, 30, 0, 2, 11, 2020);
}

void loop() {
  time_t t = now();
  String date = String(day(t)) + "-" + String(month(t)) + "-" + String(year(t));
  Serial.println(date);
  delay(1000);
}
``` 

Koodin tuloste:
`2-11-2020`

## Syvällinen tarkastelu

Päivämäärän muuntaminen merkkijonoksi on yleinen käytäntö, kuuluipa se sitten historiankirjoitukseen tai ei. Se antaa ohjelmoijille suuremman joustavuuden päivämäärien käsittelyssä ja tulostamisessa. 

Vaihtoehtoisesti, voisit käyttää sprintf-funktiota samaan tarkoitukseen, mutta String-luokka tarjoaa helpomman tavan yhdistää datatyyppejä.

Tyypillinen tapa toteuttaa päivämäärän muunnos merkkijonoksi on lisätä päivä, kuukausi ja vuosi yhteen, mutta voit myös muuttaa sen oman tarpeesi mukaan - päätät itse, minkä tyyppinen esitystapa sopii parhaiten sovellukseesi.

## Katso myös

Voit tarkistaa lisätietoa Arduino String -luokasta ja aikakirjaston (TimeLib) käytöstä seuraavista lähteistä:

- [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Time Library](https://www.pjrc.com/teensy/td_libs_Time.html)