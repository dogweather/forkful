---
title:    "Arduino: Säännöllisten lausekkeiden käyttö"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Arduino-ohjelmoinnissa?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja Arduino-ohjelmoijille. Ne mahdollistavat merkkijonojen tarkastelun ja käsittelyn monimutkaisilla tavoilla, mikä on erityisen hyödyllistä, kun halutaan etsiä tiettyä merkkijonoa vaikkapa sensorilta saadusta datasta.

## Miten käyttää säännöllisiä lausekkeita Arduinolla?

Arduino-ohjelmoinnissa säännöllisiä lausekkeita käytetään `regex`-kirjaston avulla. Kirjasto tarjoaa funktioita, joilla voidaan määrittää haluttu säännöllinen lauseke ja etsiä se merkkijonosta. Alla on esimerkki koodista, jolla etsitään säännöllisellä lausekkeella `"lämpötila:"` alkavaa merkkijonoa:

```Arduino
#include <Regex.h>

String data = "lämpötila: 25°C";
Regex regex("lämpötila: \[0-9]+°C");

if (regex.match(data)) {
  // Säännöllinen lauseke löytyi, tulostetaan tulos
  Serial.println(regex.matched());
} else {
  // Säännöllistä lauseketta ei löytynyt
  Serial.println("Ei löytynyt.");
}
```

Koodin tuloste on `"lämpötila: 25°C"`.

## Syvällisempi sukellus säännöllisiin lausekkeisiin

Mikäli haluat käyttää säännöllisiä lausekkeita Arduino-ohjelmoimissa vielä monimutkaisemmin, voit tutustua `regex`-kirjaston [dokumentaatioon](https://github.com/nickgammon/Regex). Sieltä löytyy lisätietoa mm. funktioista ja säännöllisten lausekkeiden merkinnöistä.

## Katso myös

- [regex-kirjasto Arduinolle](https://github.com/nickgammon/Regex)
- [Säännölliset lausekkeet käyttöoppaan alkuunpano](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/regex/)

Kiitos, että luit tämän artikkelin säännöllisistä lausekkeista Arduino-ohjelmoimisessa. Toivottavasti tämä auttaa sinua hyödyntämään tätä voimakasta työkalua omassa koodissasi!