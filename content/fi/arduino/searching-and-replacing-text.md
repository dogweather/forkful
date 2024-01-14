---
title:                "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi: Miksi etsiä ja korvata tekstiä Arduino-ohjelmoinnissa?

Etsiminen ja korvaaminen ovat tärkeitä toimintoja missä tahansa ohjelmoinnissa, myös Arduino-ohjelmoinnissa. Ne voivat auttaa säästämään aikaa ja vaivaa, kun haluamme muuttaa tekstin esimerkiksi useissa tiedostoissa samanaikaisesti. Ohjelmoinnissa on tärkeää tehdä prosessi mahdollisimman tehokkaaksi, ja etsimisen ja korvaamisen hyödyntäminen on yksi tapa tehdä niin Arduino-ohjelmoinnissa.

## Miten: Esimerkki etsimisen ja korvaamisen käytöstä Arduinon ohjelmoinnissa

Arduino-ohjelmoinnissa voimme käyttää sisäänrakennettua "String" -luokkaa etsimään ja korvaamaan tekstiä. Alla on esimerkki, jossa etsimme ja korvaamme tekstiä "hello" tekstin "world" kanssa:

```Arduino
#include <string.h>

String myString = "Hello, world!";
myString.replace("hello", "world");

Serial.println(myString);
```

Tämä koodi tulostaa "Hello, world!", sillä se on korvannut "hello" tekstillä "world". Voimme myös etsiä ja korvata tekstiä tietystä kohdasta, kuten seuraavassa esimerkissä:

```Arduino
#include <string.h>

String myString = "Hello there, world!";
myString.replace(6, 11, "friends");

Serial.println(myString);
```

Tämä koodi tulostaa "Hello friends, world!", sillä se on korvannut tekstin "there" tekstillä "friends" välillä 6-11. Huomaa, että merkkijonon indeksointi alkaa numerosta 0, joten "H" on indeksissä 0 ja "d" on 14.

## Syvempi sukellus: Etsimisen ja korvaamisen tekniikat Arduino-ohjelmoinnissa

Etsiminen ja korvaaminen voidaan toteuttaa myös käyttämällä erilaisia kirjastoja, kuten "TextUtils". Tämän kirjaston avulla voimme käyttää säännöllisiä lausekkeita ja muita kehittyneitä tekniikoita tekstimuutoksien tekemiseen. Lisäksi voimme käyttää for-silmukoita ja merkkijonon jakamista toiminnoissa, jotka sisältävät tekstin muokkaamista. On tärkeää tutkia erilaisia tekniikoita ja kirjastoja, jotta voimme valita parhaan työkalun halutun tuloksen saavuttamiseksi.

## Katso myös

- [String.replace() - Arduino Reference (englanniksi)](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [TextUtils - Arduino Library (englanniksi)](https://www.arduinolibraries.info/libraries/text-utils)
- [Regular Expressions -w3schools (englanniksi)](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)