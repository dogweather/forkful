---
title:                "Arduino: Päivämäärän laskeminen tulevaisuudesta tai menneisyydestä."
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit laskea tulevan tai menneen päivämäärän Arduino-ohjelmoinnilla? Päivämäärien laskeminen voi olla hyödyllistä esimerkiksi aikaperusteisissa projekteissa tai tuotteissa, kuten herätyskelloissa tai ajastimissa.

## Mitä tarvitset

Ennen kuin aloitat päivämäärien laskemisen Arduinolla, tarvitset tietoa millaisessa muodossa päivämäärät ovat ja millainen tulos haluat saavuttaa. Tarvitset myös perustiedot Arduino-ohjelmoinnista, kuten muuttujien ja ehtolauseiden käytöstä.

## Miten tehdä

Päivämäärien laskeminen tulevaisuuteen tai menneeseen päin tapahtuu samalla periaatteella. Käytetään hyväksi yhteenlaskua ja vähennystä, sekä Arduinon valmiita funktioita datan käsittelemiseen.

Seuraavassa esimerkissä lasketaan päivämäärä viikon päästä tämänhetkisestä. Käytetään funktiota `dayOfWeek()` hankkimaan tämänhetkinen viikonpäivä ja muuttujaa `daysToAdd` päivien lukumäärän säilömiseen. Lopuksi tulostetaan tulevan päivämäärän viikonpäivä ja päivämäärä muodossa `viikonpäivä, Month-Päivä-Vuosi`.

```Arduino
int day = dayOfWeek();
int daysToAdd = 7;
int futureDay = (day + daysToAdd) % 7;

Serial.print("Tuleva päivämäärä: ");
switch (futureDay) {
  case 0: Serial.print("Sunnuntai, ");
          break;
  case 1: Serial.print("Maanantai, ");
          break;
  case 2: Serial.print("Tiistai, ");
          break;
  case 3: Serial.print("Keskiviikko, ");
          break;
  case 4: Serial.print("Torstai, ");
          break;
  case 5: Serial.print("Perjantai, ");
          break;
  case 6: Serial.print("Lauantai, ");
          break;
}
Serial.print(month());
Serial.print("-");
Serial.print(day()+daysToAdd);
Serial.print("-");
Serial.println(year());
```

Esimerkkitulos:

```
Tuleva päivämäärä: Maanantai, 06-22-2020
```

## Syvemmälle aiheeseen

Monimutkaisemmissa päivämäärien laskemiseen liittyvissä tehtävissä voi tarvita hieman enemmän pohjatyötä. Esimerkiksi ottaessa huomioon karkausvuodet tai aikavyöhykkeet voi vaatia lisäfunktioita ja tarkempaa laskemista. On tärkeää ymmärtää tiettyjen kuukausien pituus ja millainen vaikutus niillä voi olla laskettaessa päivämääriä.

## Katso myös

- [Arduino Reference - Date and Time](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorialspoint - Arduino Date and Time Functions](https://www.tutorialspoint.com/arduino/arduino_date_time_functions.htm)
- [Instructables - How to Work with Date and Time in Arduino](https://www.instructables.com/id/How-to-Work-With-Dates-and-Times-in-Arduino/)