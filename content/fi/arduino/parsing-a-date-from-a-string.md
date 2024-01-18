---
title:                "Päiväyksen erottaminen merkkijonosta"
html_title:           "Arduino: Päiväyksen erottaminen merkkijonosta"
simple_title:         "Päiväyksen erottaminen merkkijonosta"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Parsing on prosessi, jossa pätkä tekstiä erotetaan osiin ja käsitellään sitä sen jälkeen tarkemmin. Parsing päivämäärää merkkijonosta tarkoittaa päivämäärän erottamista merkkijonosta ja sen muuntamista käytettävään muotoon. Tämä on hyödyllistä ohjelmoijille, kun he haluavat käsitellä päivämääriä ohjelmointikoodissaan helposti ja tarkasti.

## Kuinka tehdä?

Arduino-koodiesimerkit:

```Arduino 
// Luodaan merkkijono, joka sisältää päivämäärän.
String date = "30.4.2021";

// Erotetaan päivämäärä osiin ja tallennetaan muuttujiin.
int day = date.substring(0, 2).toInt();
int month = date.substring(3, 5).toInt();
int year = date.substring(6).toInt();

// Tulostetaan päivämäärä.
Serial.print(day);
Serial.print('.');
Serial.print(month);
Serial.print('.');
Serial.print(year);

// Output: 30.4.2021
```

Toinen vaihtoehtoinen tapa on käyttää Arduino-kirjastoa nimeltään "Date Strings". Sen avulla voit muuntaa merkkijonossa olevan päivämäärän suoraan tarkempaan muotoon, kuten Unix-aikaleimaksi.

```Arduino
// Luodaan merkkijono, joka sisältää päivämäärän.
String date = "30.4.2021";

// Muuntaa merkkijonon Unix-aikaleimaksi.
unsigned long unixTime = DateStrings::dateTimeToUnixTime(date.c_str());

// Tulostetaan Unix-aikaleima.
Serial.print(unixTime);

// Output: 1619772000
```

## Syvällinen sukellus

Päivämäärän erottaminen merkkijonosta ei ole uusi asia ohjelmoinnissa. Joissakin vanhemmissa ohjelmointikielissä, kuten C, tämä oli tehtävä käyttämällä melko monimutkaisia funktioita. Nykyään on kuitenkin olemassa paljon kevyempiä ja helposti ymmärrettäviä vaihtoehtoja, kuten Arduino-kirjastot ja valmiita funktioita.

Yksi vaihtoehto on käyttää "time.h" -kirjastoa, joka sisältää valmiita funktioita päivämäärän muuntamiseksi eri muotoihin. Toisaalta Date Strings -kirjasto on kehitetty erityisesti Arduinoa varten ja se tarjoaa käteviä toimintoja päivämäärän käsittelyyn.

Implementointitavoista riippumatta on tärkeää varmistaa, että käytettävä merkkijono on oikeassa muodossa ja että päivämäärittelyssä käytetään oikeaa formaattia (esim. DD.MM.YYYY). Muuten tulokset voivat olla virheellisiä.

## Katso myös

Voit tutustua tarkemmin "Date Strings" -kirjastoon ja sen käyttöön täältä: https://github.com/arduino-libraries/DateStrings

Lisätietoja "time.h" -kirjastosta löydät täältä: https://www.arduino.cc/reference/en/libraries/time/