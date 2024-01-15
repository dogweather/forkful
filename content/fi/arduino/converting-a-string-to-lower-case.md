---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi? Voit esimerkiksi haluta verrata syötteitä ilman että välitä eri kirjainkoosta tai kirjoittaa käyttäjälle ymmärrettävässä muodossa.

## Kuinka

Muuttaaksesi merkkijonon pieniksi kirjaimiksi, käytä `toLowerCase()` -funktiota. Esimerkiksi:

```Arduino
String nimi = "TEKSTI";
Serial.println(nimi.toLowerCase());
```

Tämä tulostaisi "teksti" sarjamonitorille.

## Syvempää sukellusta

`toLowerCase()`-funktio ei muuta alkuperäistä merkkijonoa, vaan luo uuden muunnetun version. Tämä mahdollistaa alkuperäisen merkkijonon muuttumattomuuden. Funktion sisällä käytetään ASCII-kooditaulukkoa, jotta jokainen kirjain voidaan muuntaa vastaavaksi pieneksi kirjaimeksi.

## Katso myös

- [Official Arduino Reference: `toLowerCase()`](https://www.arduino.cc/reference/en/language/functions/string/functions/tolowercase/)
- [ASCII-kooditaulukko](https://www.ascii-code.com/)