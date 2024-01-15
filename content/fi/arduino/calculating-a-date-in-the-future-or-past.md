---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen"
html_title:           "Arduino: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Miksi
## Miksi laskemme tulevaisuuden tai menneen päivämäärän

Päivämäärien laskeminen voi olla hyödyllistä esimerkiksi tapahtumien aikatauluttamisessa, kuten esimerkiksi syntymäpäivien tai lomien suunnittelussa.

# Miten tehdä se

Päivämäärän laskemiseen Arduino-ohjelmointiympäristössä tarvitsemme ensin muuttujia päivämäärän, kuukauden ja vuoden arvoille. Datan esimerkiksi käyttäjän antamat tiedot voidaan tallentaa näihin muuttujiin. Tämän jälkeen voimme käyttää valmiita funktioita, kuten esimerkiksi ```dayOfWeek``` tai ```dayOfYear```, laskemaan halutun päivämäärän tulevaisuudessa tai menneisyydessä. Alla on esimerkki koodista, joka laskee päivämäärän 10 päivää eteenpäin:

```
Arduino.init();

int paivamaara = 10;
int kuukausi = 4;
int vuosi = 2020;

int tulevaPaivamaara = dayOfMonth(paivamaara, kuukausi, vuosi) + 10;

Serial.println(tulevaPaivamaara);
```

Tämä koodi tulostaa sarjaporttiin päivämäärän 10 päivää laskettuna nykyisestä päivämäärästä. Voit muuttaa päivämäärää, kuukautta ja vuotta haluamiksesi ja kokeilla erilaisia laskutoimituksia.

# Syvemmälle

Voidaksemme laskea tulevaisuuden tai menneen päivämäärän tarkemmin, voimme käyttää lisää funktioita ja laskutoimituksia. Esimerkiksi voimme käyttää ```year```-funktiota saadaksemme käyttäjän antamasta vuodesta esimerkiksi kuluvan vuoden tai laskemalla montako vuotta on jäljellä annettuun päivämäärään. Lisäksi voimme käyttää ```month```-funktiota saadaksemme käyttäjän antamasta kuukaudesta esimerkiksi kuluvan kuukauden tai laskemalla kuukausien määrän jäljellä olevaan päivämäärään.

Ole kuitenkin tarkkana, sillä päivämäärät voivat vaihdella eri mantereilla ja niiden laskeminen voi olla monimutkaista. On tärkeää ottaa huomioon myös karkausvuodet ja muut kalenterissa tapahtuvat muutokset.

# Katso myös

- [Official Arduino Website](https://www.arduino.cc/)
- [Aloittelijan opas Arduino-ohjelmointiin](https://create.arduino.cc/projecthub/Aritro/getting-started-with-arduino-36f729)
- [Päivämäärän laskeminen C-kielellä](https://www.programiz.com/c-programming/library-function/stdio.h/tm-gmtime)