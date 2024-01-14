---
title:                "Arduino: Tekstin etsiminen ja korvaaminen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, jolloin Arduinon ohjelmassa täytyy tehdä tekstin etsimistä ja korvaamista. Tämä voi olla tarpeellista esimerkiksi jotta saadaan haluttu muotoilu tai tietynlainen tietojen käsittely.

## Kuinka

Arduino tarjoaa useita tapoja etsiä ja korvata tekstiä. Yksi tapa on käyttää `replace()`-funktiota, joka korvaa kaikki halutut merkkijonot toisilla. Esimerkiksi seuraava koodinpätkä korvaa kaikki "Hei" sanat "Moi" sanoiksi:

```Arduino
String teksti = "Hei maailma! Hei kaikille!";
teksti.replace("Hei", "Moi");
Serial.println(teksti); //tulostaa "Moi maailma! Moi kaikille!"
```

Toinen tapa on käyttää `indexOf()` ja `substring()` -funktioita yhdessä. `indexOf()` etsii haluttua merkkijonoa ja palauttaa sen indeksin, kun taas `substring()` hakee tietyn alueen merkkijonosta. Seuraava koodinpätkä korvaa ensimmäisen "Hei" sanan "Moi" sanalla:

```Arduino
String teksti = "Hei maailma! Hei kaikille!";
int indeksi = teksti.indexOf("Hei"); //etsii "Hei" sanan indeksin
teksti = teksti.substring(0, indeksi) + "Moi" + teksti.substring(indeksi + 3);
//otetaan tekstistä osat ennen ja jälkeen "Hei" sana ja yhdistetään ne "Moi" sanaan
Serial.println(teksti); //tulostaa "Moi maailma! Hei kaikille!"
```

## Syvempi sukellus

Etsimistä ja korvaamista tekstistä voidaan tehdä myös käyttämällä vakiona olevia String funktioita, kuten `startsWith()` ja `endsWith()` sekä `replaceFirst()` ja `replaceAll()`. Näitä funktioita kannattaa tutkia lisää voidakseen valita parhaan vaihtoehdon kuhunkin tilanteeseen.

## Katso myös

- Arduino String-funktioiden opas: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Vakiona olevat String-objektit: https://www.arduino.cc/en/Tutorial/TextStringObjects