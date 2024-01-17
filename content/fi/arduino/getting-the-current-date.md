---
title:                "Nykyisen päivämäärän hakeminen"
html_title:           "Arduino: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Päivämäärän saaminen tarkoittaa päivämäärän, kuukauden ja vuoden lukemista ja tallentamista järjestelmään. Ohjelmoijat tekevät tämän, jotta he voivat käyttää näitä tietoja esimerkiksi päivämäärän näyttämiseen näytöllä tai tallentamaan tietoja tietokantaan.

# Miten:

Esimerkit ja tulosteet:

```
Arduino ohjelmointi tänään void setup() {
    Serial.begin(9600); // Asetetaan sarjaportti nopeudelle 9600
    delay(1000); // Pieni viive, jotta sarjaportti ehtii käynnistyä
}
 
void loop() {
    // Luodaan muuttujat, joihin tallennetaan päivämäärä, kuukausi ja vuosi
    int day, month, year;
    day = day(); // Haetaan päivämäärä
    month = month(); // Haetaan kuukausi
    year = year(); // Haetaan vuosi
 
    // Tulostetaan päivämäärä sarjaporttiin
    Serial.print("Tänään on ");
    Serial.print(day);
    Serial.print(".");
    Serial.print(month);
    Serial.print(".");
    Serial.println(year);
 
    delay(1000); // Odottaa sekunnin ennen seuraavaa lukemista
}
```

Tuloste:
```
Tänään on 3.4.2020
```
 
# Syväsukellus:

Jotkut vaihtoehdot päivämäärän saamiseen voivat sisältää käyttäjän syöttöjä tai tietojen tarkempaa tallentamista. Tämä esimerkki olettaa, että Arduino laite on liitetty internetiin ja se voi hakea ajantiedot verkosta.

Päivämäärän haku perustuu yleensä järjestelmän sisäiseen kellopiiriin (RTC), joka säilyttää ajan- ja päivämäärätietoja laitteen sähkökatkon aikana. Arduino-oppaassa on lisätietoja siitä, miten voit käyttää RTC:tä päivämäärän saamiseksi.

# Katso myös:

Lisätietoja RTC:stä: https://www.arduino.cc/en/tutorials/time/keeping-time