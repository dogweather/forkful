---
title:                "Arduino: Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Arduino-ohjelmointi sisältää monia erilaisia projekteja ja tehtäviä, joissa saattaa olla tarve esittää päivämäärä käyttäjälle tai tallentaa päivämäärä johonkin muuhun tarkoitukseen. Muutos päivämäärästä merkkijonoksi on yksinkertainen mutta tärkeä taito, jonka avulla voit tehdä monenlaisia projekteja.

## Miten

Aloitetaan yksinkertaisella esimerkillä, jossa tulostetaan nykyinen päivämäärä käyttäjälle merkkijonona käyttäen `day()`, `month()` ja `year()` funktioita:

```Arduino
int day = day(); // tallennetaan päivämäärä muuttujaan
int month = month(); // tallennetaan kuukausi muuttujaan
int year = year(); // tallennetaan vuosi muuttujaan

String date = String(day) + "/" + String(month) + "/" + String(year); // muutetaan päivämäärä merkkijonoksi ja tallennetaan se uuteen muuttujaan
Serial.println(date); // tulostetaan päivämäärä sarjaporttiin
```

Kun koodin ajaa, tulostuu päivämäärä merkkijonona muodossa "day/month/year" esimerkiksi sarjaporttiin.

Voit myös valita minkä tahansa muun formaatin muutetulle päivämäärälle käyttämällä DateTime-kirjaston `DateTime::toString()` funktiota. Esimerkiksi:

```Arduino
#include <DateTime.h> // otetaan käyttöön DateTime-kirjasto

DateTime now; // luodaan muuttuja, johon tallennetaan nykyinen aika

String date = now.toString("DD.MM.YYYY"); // muutetaan päivämäärä merkkijonoksi toivotussa formaatissa ja tallennetaan se muuttujaan
Serial.println(date); // tulostetaan päivämäärä sarjaporttiin
```

Tämä koodi tulostaa päivämäärän muodossa "DD.MM.YYYY" esimerkiksi "25.11.2021".

## Syvempi sukellus

Tarkastellaan hieman tarkemmin kuinka päivämäärä saadaan muutettua merkkijonoksi käyttäen DateTime-kirjaston `DateTime::toString()` funktiota. Tämä funktio ottaa vastaan parametrina merkkijonon, joka määrittelee halutun formaatin. Esimerkiksi "DD.MM.YYYY" käyttäessä saadaan päivämäärä muodossa "DD.MM.YYYY".

Muita mahdollisia parametreja ovat esimerkiksi "D", joka vastaa viikonpäivää numerolla (1-7), ja "M", joka vastaa kuukauden nimeä lyhennettynä (esim. "Jan" tammikuulle). Voit tutustua kaikkiin mahdollisiin parametreihin DateTime-kirjaston dokumentaatiosta.

## Katso myös

- [DateTime-kirjaston dokumentaatio](https://github.com/PaulStoffregen/DateTime)
- [Arduino DateTime-esimerkkejä](https://playground.arduino.cc/Code/DateTime/)
- [Muita Arduino-ohjelmointiin liittyviä artikkeleita (englanniksi)](https://blog.arduino.cc/tag/code/)