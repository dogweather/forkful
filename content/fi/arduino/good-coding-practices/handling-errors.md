---
title:                "Virheiden käsittely"
aliases: - /fi/arduino/handling-errors.md
date:                  2024-01-26T00:50:07.844104-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden käsittely ohjelmissasi nappaa kiinni ne odottamattomat asiat, jotka yrittävät kaataa suunnitelmasi. Sen tarkoitus on estää Arduinoasi sekoamasta, kun odottamaton tapahtuu.

## Kuinka:

Kuvitellaan, että Arduinosi lukee sensorilta, joka voi ajoittain tuottaa mittausalueen ulkopuolisia arvoja. Tässä on tapa, jolla voisit käsitellä tätä:

```Arduino
int sensorValue = analogRead(A0);

if (sensorValue >= 0 && sensorValue <= 1023) {
  // Arvo on mittausalueella, jatketaan käsittelyä
  Serial.println(sensorValue);
} else {
  // Arvo on mittausalueen ulkopuolella, käsittele virhe
  Serial.println("Error: Sensor value out of range.");
}
```
Esimerkkitulostus:
```
523
Error: Sensor value out of range.
761
```

## Syväsukellus

Virheiden käsittely ei aina ole ollut näin suoraviivaista. Varhaisina aikoina kehittäjät usein jättivät virheet huomioimatta, mikä johti pelättyyn "määrittelemättömään käytökseen". Ohjelmoinnin kehittyessä myös työkalut kehittyivät — nykyään sinulla on poikkeuksia monissa kielissä, mutta Arduinossa noudatetaan vielä vanhan koulukunnan 'tarkista ensin' -periaatetta laitteistorajoitusten ja C++:n juurien vuoksi.

Arduino-ohjelmoinnissa näkee usein `if-else`-lauseita virheiden käsittelyyn. On kuitenkin vaihtoehtoja: `assert`-funktion käyttäminen suorituksen pysäyttämiseen, jos ehto ei täyty, tai vikasietoisten mekanismien suunnittelu itse laitteistoon.

Virheiden käsittelyn toteuttamisessa harkitse ohjelman pysäyttämisen ja sen jatkamisen vaikutusta oletusarvoiseen tai turvalliseen tilaan. On kompromissi, ja oikea valinta riippuu keskeytysten potentiaalisesta vahingosta verrattuna virheelliseen toimintaan.

## Katso Myös

Syvennä tietämystäsi virheenilmaisusta ja -käsittelystä näiden avulla:

- Arduino Kieli Viite: https://www.arduino.cc/reference/en/
- Embedded Artistryn syvempi katsaus virheiden käsittelyyn: https://embeddedartistry.com/blog/2017/05/17/creating-a-circular-buffer-in-c-and-c/
- C++ Virheenkäsittely: https://en.cppreference.com/w/cpp/error/exception

Tämän tiedon avulla sinulla pitäisi olla taitoja ja itseluottamusta välttää Arduinon seikkailuissasi virheiden aiheuttamat ansoja.
