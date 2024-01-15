---
title:                "Kuviota vastaavien merkkien poistaminen"
html_title:           "Arduino: Kuviota vastaavien merkkien poistaminen"
simple_title:         "Kuviota vastaavien merkkien poistaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Tämä voi olla hyödyllistä esimerkiksi, jos sinulla on tekstimuotoinen data ja haluat puhdistaa sen ennen sen käyttöä.

## Kuinka tehdä

Voit poistaa merkkejä, jotka vastaavat tiettyä kaavaa, käyttämällä ```Arduino``` -koodia ja ```replace()``` -funktiota. Esimerkiksi, jos haluat poistaa kaikki numerot merkkijonosta, voit käyttää seuraavaa koodia:

```
Arduinostring data = "Hei 123 maailma!";
data.replace("123", "");
```

Koodin jälkeen muuttuja ```data``` sisältäisi merkkijonon "Hei maailma!". Notice that the numbers have been deleted.

## Syväsyventyminen

Funktio ```replace()``` toimii poistamalla kaikki merkkijonot, jotka vastaavat annettua kaavaa. Voit myös korvata ne toisella merkkijonolla antamalla kaksi parametria ```replace()``` -funktiolle. Esimerkiksi, ```data.replace("123", "ABC")``` korvaisi merkkijonon "123" merkkijonolla "ABC".

## Katso myös

- Arduino ```replace()``` -funktio: https://www.arduino.cc/reference/en/language/functions/strings/stringobject/replace/
- Arduino merkkijonot: https://www.arduino.cc/reference/en/language/variables/data-types/string/