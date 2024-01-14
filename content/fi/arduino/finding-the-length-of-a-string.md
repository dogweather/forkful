---
title:                "Arduino: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Arduino - Kuinka löytää merkkijonon pituus

## Miksi?

Monissa ohjelmoinnin projekteissa saattaa olla tarvetta saada selville merkkijonon pituus. Se voi olla tarpeellista esimerkiksi tekstien käsittelyssä tai erilaisten syötteiden tarkistuksessa. Onneksi Arduinoilla tämä on helppoa ja tänään me katsomme kuinka se tehdään.

## Kuinka?

```Arduino
String merkkijono = "Tervetuloa lukijat!";

int pituus = merkkijono.length(); //palauttaa pituuden, joka tässä tapauksessa on 21

Serial.println(pituus); //tulostaa 21 sarjaporttiin
```

Näin yksinkertaisesti voit saada selville merkkijonon pituuden. Ensimmäisessä rivissä luodaan String-olio, johon tallennetaan käsiteltävä merkkijono. Toisessa rivissä käytetään `length()`-metodia, joka palauttaa merkkijonon pituuden. Lopuksi tulostetaan tulos sarjaporttiin. Kokeile muuttaa merkkijonoa ja huomaa miten pituus muuttuu.

Jos haluat tarkistaa merkkijonon pituuden ennen sen tallentamista String-olioon, voit käyttää `strlen()`-funktiota. Se palauttaa merkkijonon pituuden numerona, joten se ei toimi suoraan String-olioiden kanssa.

```Arduino
char merkkijono[] = "Hei maailma!";

int pituus = strlen(merkkijono); //palauttaa 12

Serial.println(pituus); //tulostaa 12 sarjaporttiin
```

## Syventyvä matka

Merkkijonon pituuden löytämiseen liittyy muutamia tärkeitä asioita, joita kannattaa pitää mielessä. Ensinnäkin, String-oliot käyttävät muistia tehokkaammin ja niiden käsittely on helpompaa kuin tavallisilla merkkijonoilla. Toiseksi, merkkijonon pituus ei sisällä nollamerkkiä, joten esimerkiksi merkkijonon "Hello" pituus on 5 eikä 6.

Kannattaa myös huomioida, että String-olioon liittyy lukuisia muita hyödyllisiä metodeja, joita kannattaa tutkia, kuten `substring()` ja `charAt()`, joilla voi esimerkiksi jakaa merkkijonon osiin tai etsiä tiettyjä merkkejä.

## Katso myös

- [Arduino String Documentation](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [C String Functions](https://www.programiz.com/c-programming/library-function/string.h)
- [Arduino String vs. C String](https://forum.arduino.cc/index.php?topic=255361.0)