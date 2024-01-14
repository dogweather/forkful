---
title:    "Arduino: Alimerkkijonojen erottaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit erottaa osamerkkijonoja? Joskus on tarpeen käsitellä tekstiä osa kerrallaan ja tällöin osamerkkijonojen erottaminen on tarpeen. Tämä on erityisen hyödyllistä, jos haluat manipuloida tietyillä merkkijonoilla, kuten etsiä tiettyjä sanoja tai numeroita.

## Ohjeet

Kätevä tapa erottaa osamerkkijonoja Arduino-ohjelmointiympäristössä on käyttää string-luokan substring()-funktiota. Tämä funktio ottaa kaksi parametria: aloitusindeksin ja lopetusindeksin. Näiden avulla voit määrittää, mikä osamerkkijono sinua kiinnostaa. Esimerkissä käytämme string-muuttujaa, mutta voit käyttää myös char-muuttujia. Huomaa myös, että indeksit alkavat nollasta.

```Arduino
String teksti = "Tämä on esimerkkilause.";
String osa = teksti.substring(5, 9);
```

Yllä olevassa koodissa etsimme osamerkkijonon "on" tekstin "esimerkkilause" sisältä ja tallennamme sen muuttujaan "osa". Voit myös käyttää negatiivisia indeksejä, jolloin tekstiä lasketaan takaa päin.

```Arduino
String teksti = "Tämä on esimerkkilause.";
String osa = teksti.substring(-9, -5);
```

## Syväsukellus

Voit myös käyttää substring()-funktiota saadaksesi käyttöösi vain tietyn määrän merkkejä ilman, että sinun tarvitsee tietää indeksejä. Tässä käytämme funktiota, joka ottaa parametreiksi aloitusindeksin ja halutun merkkimäärän.

```Arduino
String teksti = "Tämä on esimerkkilause.";
String osa = teksti.substring(5, 2);
```

Tämä palauttaa merkkijonon "on", koska aloitusindeksistä alkaen otetaan kaksi merkkiä.

## Katso myös

- Arduino string-luokan dokumentaatio: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Ohjeita merkkijonojen käsittelyyn Arduinolla: https://www.arduino.cc/en/Tutorial/StringIndexOf