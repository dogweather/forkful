---
title:                "Arduino: Merkkijonon suurtaminen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi Arduino-ohjelmassa? Joskus haluat esimerkiksi näyttää tulostimessa käyttäjän syöttämän nimen, mutta haluat, että nimi näkyy oikeassa muodossa isolla alkukirjaimella. Tässä tapauksessa voi olla hyödyllistä käyttää merkkijonon kapselointitoimintoa.

## Miten se tehdään
Voit helposti muuttaa merkkijonon ensimmäisen kirjaimen isoksi Arduino-ohjelmassa käyttämällä "capitalize" -toimintoa. Katso alla oleva esimerkki, kuinka tämä voidaan toteuttaa:

```Arduino
// Määritetään merkkijono
String nimi = "alex";

// Käytetään capitalize-toimintoa muuttamaan nimen ensimmäinen kirjain isoksi
nimi.capitalize();

// Tulostetaan muokattu nimi
Serial.println(nimi); // Tulostaa "Alex"
```

## Syvä sukellus
Merkkijonon kapselointitoiminto käyttää C ++: n standardikirjaston "string.h" -kirjastoa. Tämä kirjasto sisältää useita toimintoja, jotka voivat manipuloida merkkijonon merkkejä.

Yksi tällainen toiminto on "toupper ()", joka muuttaa merkkijonon kaikki kirjaimet isoksi kirjoitettuiksi. "capitalize" -toiminto käyttää tätä toimintoa muuttaa ensimmäisen kirjaimen isoksi ja jättää muut kirjaimet ennalleen.

## Katso myös
- [Arduino-opetusohjelma: Merkkijonojen käsittely ja manipulointi](https://create.arduino.cc/projecthub/Arduino_Genuino/strings-937228)
- [C ++: n "string.h" -kirjaston dokumentaatio](https://www.cplusplus.com/reference/cstring/)
- [capitalize-toiminnon dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/)