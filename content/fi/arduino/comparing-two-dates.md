---
title:                "Kahden päivämäärän vertailu"
html_title:           "Arduino: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

Ymmärrä kahden päivämäärän vertailu Arduino-ohjelmoinnissa

## Mitä ja miksi?

Kahden päivämäärän vertailu on prosessi, jossa verrataan kahta päivämäärää ja selvitetään, kumpi niistä on suurempi tai pienempi. Tämä on tärkeää ohjelmoijille, jotka tarvitsevat tapoja hallita päivämäärätietoja ja tehdä päätöksiä niiden perusteella.

## Miten:

Seuraavassa on kaksi esimerkkikoodia vertailemaan kahta päivämäärää ja tulostamaan tulos:

Arduino-koodiesimerkki 1:

```Arduino
int date1 = 20210805; // ensimmäinen päivämäärä muodossa vuosi, kuukausi, päivä
int date2 = 20210810; // toinen päivämäärä muodossa vuosi, kuukausi, päivä

if (date1 < date2) { // tarkistetaan, onko date1 pienempi kuin date   
  Serial.println("Ensimmäinen päivämäärä on pienempi kuin toinen päivämäärä!");
} else if (date1 == date2) { // tarkistetaan, ovatko päivämäärät samat
  Serial.println("Päivämäärät ovat samat!");
} else { // jos edelläolevat ehdot eivät täyty, date1 on suurempi kuin date2
  Serial.println("Ensimmäinen päivämäärä on suurempi kuin toinen päivämäärä!");
}
```

Tulostus:

```
Päivämäärät ovat samat!
```

Arduino-koodiesimerkki 2:

```Arduino
String date1 = "2021-08-05"; // ensimmäinen päivämäärä merkkijonona
String date2 = "2021-08-10"; // toinen päivämäärä merkkijonona

if (date1 < date2) { // tarkistetaan, onko date1 pienempi kuin date2
  Serial.println("Ensimmäinen päivämäärä on pienempi kuin toinen päivämäärä!");
} else if (date1 == date2) { // tarkistetaan, ovatko päivämäärät samat
  Serial.println("Päivämäärät ovat samat!");
} else { // jos edelläolevat ehdot eivät täyty, date1 on suurempi kuin date2
  Serial.println("Ensimmäinen päivämäärä on suurempi kuin toinen päivämäärä!");
}
```

Tulostus:

```
Ensimmäinen päivämäärä on pienempi kuin toinen päivämäärä!
```

## Syvemmälle:

Päivämäärän vertailu on ollut tärkeä osa ohjelmointia jo pitkään ja sitä on käytetty monilla eri ohjelmointikielillä. Arduino-kirjastot, kuten "Time" ja "RTCLib", tarjoavat valmiita toimintoja päivämäärien vertailuun.

On myös olemassa muita tapoja vertailla päivämääriä, kuten käyttämällä Unix-timestamp-työkalua tai muuntamalla päivämäärät kokonaislukumuotoon ja vertailemalla niitä.

Päivämäärien vertailuun voi vaikuttaa myös aikavyöhykkeet ja kesäaika, jotka on huomioitava vertaillessa päivämääriä.

## Katso myös:

1. [Time - Arduino-kirjasto päivämäärien hallintaan] (https://www.arduino.cc/en/reference/time)
2. [RTCLib - Arduinon kirjasto RTC piirien käyttöön] (https://www.arduino.cc/en/reference/rtclib)