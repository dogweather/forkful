---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Jokaiselle, joka viettää paljon aikaa ohjelmien kirjoittamiseen, voi olla hyödyllistä oppia etsimään ja korvaamaan tekstiä. Tämä voi auttaa säästämään aikaa ja välttämään virheitä.

## Kuinka

Arduino-ohjelmistossa on sisäänrakennettuna ```replace()``` -funktio, joka mahdollistaa tehokkaan ja helpon tekstien etsimisen ja korvaamisen. Funktion syntaksi on seuraava:

```Arduino
replace(alkuperäinen_teksti, etsittävä_teksti, korvaava_teksti);
```

Esimerkiksi, jos haluat korvata kaikki "Hello" sanat "Hei", voit käyttää tätä funktiota seuraavasti:

```Arduino
replace("Hello world", "Hello", "Hei");
```

Tämä tuottaa tuloksen "Hei world".

## Syvemmälle

On tärkeää huomata, että ```replace()``` -funktio korvaa kaikki esiintymät alkuperäisessä tekstissä. Jos haluat korvata vain ensimmäisen esiintymän, voit käyttää ```replaceFirst()``` -funktiota.

Lisäksi, voit käyttää myös säännöllisiä lausekkeita etsimisen ja korvaamisen helpottamiseksi. Esimerkiksi, voit käyttää ```replace()``` -funktiota seuraavasti:

```Arduino
replace("1234ABC5678", "[0-9]", "X");
```

Tämä korvaa kaikki numerot "X":llä ja palauttaa tuloksen "XXXXABCXXXX".

## Katso myös

- [Arduino Reference - replace()](https://www.arduino.cc/reference/en/language/structure/functions/stringfunctions/replace/)
- [Arduino Tutorial - Regular Expressions](https://www.arduino.cc/en/Tutorial/RegexpReplace)
- [FreeCodeCamp - An Introduction to Regular Expressions](https://www.freecodecamp.org/news/an-introduction-to-regular-expressions-regex/)