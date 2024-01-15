---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Arduino: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitilanteissa on tarve yhdistää useita merkkijonoja yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä esimerkiksi tekstin muodostamisessa tai tiedon tallentamisessa.

## Miten

Merkkijonojen yhdistäminen voidaan tehdä käyttämällä ```Arduino``` -ohjelmoinnissa olevaa ```concat()``` -funktiota. Tämä funktio yhdistää annetut merkkijonot järjestyksessä ja palauttaa kokonaisen merkkijonon.

Esimerkki koodista:

```
String etunimi = "Matti";
String sukunimi = "Meikäläinen";
String kokoNimi = etunimi.concat(sukunimi);

Serial.print(kokoNimi); //tulostaa "MattiMeikäläinen"
```

## Syvempää tietoa

```concat()``` -funktiota voidaan käyttää myös yhdistämään enemmän kuin kaksi merkkijonoa. Esimerkiksi seuraavassa koodissa yhdistetään kolme erillistä merkkijonoa ja tulostetaan se yhdistettynä:

```
String alku = "Hei";
String keskiosa = "kaikki";
String loppu = "yhdessä!";

String yhdistetty = alku.concat(keskiosa, loppu);

Serial.print(yhdistetty); //tulostaa "Heikaikkiyhdessä!"
```

On myös huomionarvoista, että ```concat()``` -funktio ei muuta alkuperäisiä merkkijonoja, vaan luo uuden merkkijonon. Tämä tarkoittaa sitä, että alkuperäisten merkkijonojen arvot eivät muutu.

Mahdollisuus yhdistää merkkijonoja on hyödyllinen myös silloin, kun halutaan muodostaa esimerkiksi sensoritietoja tallentava CSV-tiedosto. Tällöin voidaan yhdistää eri arvot yhteen merkkijonoon ja tallentaa se tiedostoon. 

## Katso myös

- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/
- https://www.arduino.cc/reference/en/language/variables/data-types/string/
- https://www.arduino.cc/en/Tutorial/StringsConcatenation