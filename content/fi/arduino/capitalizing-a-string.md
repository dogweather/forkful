---
title:                "Arduino: Merkkijonon suurennus"
simple_title:         "Merkkijonon suurennus"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

String-muuttujien käsittely on olennainen osa monia Arduino-projekteja. Oletetaan esimerkiksi, että sinulla on sensori, joka lukee ulkolämpötilaa ja haluat tulostaa sen LCD-näytölle. Mutta ennen kuin voit näyttää lämpötilan, sinun täytyy saada se tallennettua String-muuttujaan. Mutta mitä jos haluat näyttää lämpötilan isoilla kirjaimilla, esimerkiksi "23 C"? Tässä tulee tarpeeseen stringin muuttaminen isoin kirjaimin.

## Miten

Voit tehdä tämän yksinkertaisesti kahdella tavalla: käyttämällä valmista funktiota tai kirjoittamalla oman.

### Valmis funktio

Arduino tarjoaa valmiin funktion `toUpperCase()`, joka muuntaa stringin isoihin kirjaimiin. Käyttö on yksinkertaista: anna vain muutettava stringi funktion sisälle ja tallenna uuteen muuttujaan.

```Arduino
String alkuperainenString = "23 C";
String suuriString = alkuperainenString.toUpperCase(); // "23 C" muuttuu "23 C"ksi
```

### Oma funktio

Voit myös kirjoittaa oman funktion, joka muuntaa stringin isoihin kirjaimiin. Tässä esimerkki siitä, miten se voitaisiin toteuttaa:

```Arduino
String muutaIsoksi(String pieniString) {
  String isoni = ""; // alustetaan uusi stringi

  for (int i = 0; i < pieniString.length(); i++) { // käydään läpi jokainen merkki pienessä stringissä
    isoni += toupper(pieniString.charAt(i)); // lisätään isoon stringiin jokainen merkki muutettuna isoksi
  }

  return isoni; // palautetaan uusi stringi
}

String alkuperainenString = "23 C";
String suuriString = muutaIsoksi(alkuperainenString); // "23 C" muuttuu "23 C"ksi
```

Huomaa, että oman funktion kirjoittaminen on hieman monimutkaisempaa, mutta se antaa sinulle enemmän hallintaa ja muunnoksen voi tehdä haluamallasi tavalla.

## Syväsukellus

Jotta ymmärrät paremmin, miten stringin muuntaminen isoihin kirjaimiin toimii, meidän täytyy tarkastella hieman tarkemmin, miten stringit toimivat Arduino-ohjelmoinnissa.

Stringit ovat todellisuudessa vain merkkijonoja, jotka koostuvat yksittäisistä merkeistä. Esimerkiksi "Hello" on todellisuudessa "H" + "e" + "l" + "l" + "o". Voit käyttää kahta eri tapaa käsitellä ja muokata stringejä:

- Merkkijonon ominaisuudet ja metodit: käytetään pisteota (.) stringin lopussa
- Merkkijonofunktiot Arduino.h-kirjastosta: käytetään yhdessä stringien kanssa

Kun muutat stringin isoihin kirjaimiin, käytät oikeastaan merkkijonofunktiota `toupper()`, joka ottaa parametrina merkin ja palauttaa muutetun version.

## Katso myös

- Arduino-valmistajan virallinen dokumentaatio stringien hallinnasta: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- MDN Web Docs selitys stringien hallinnasta: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String