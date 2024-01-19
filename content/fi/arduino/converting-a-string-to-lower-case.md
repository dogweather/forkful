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

## Mikä & Miksi?

Muuttujan nimeltään string muuttaminen pieniksi kirjaimiksi tarkoittaa sen kirjainten muuntamista suurista kirjaimista pieniksi. Ohjelmoijat tekevät tämän yleensä datan vertailun yhtenäistämiseksi ja helpottamiseksi.

## Näin teet:

Arduino tarjoaa toLowerCase()-funktion, jonka avulla voit muuttaa merkkijonon pieniksi kirjaimiksi. Katso esimerkki:
```Arduino
String myString = "HELLO WORLD";
myString.toLowerCase();
Serial.println(myString);
```
Tämä tulostaa:
```Arduino
"hello world"
```


## Syvempi sukellus:

Historiassa ensimmäiset ohjelmoijat huomasivat tarpeen muuttaa merkkijonot pieniksi kirjaimiksi, koska tietokoneet lukevat isot ja pienet kirjaimet eri tavoin. 

Vaihtoehtoja toLowerCase()-menetelmälle on, kuten itse kirjoittaminen funktion, joka käy läpi jokaisen merkin merkkijonossa ja muuttaa sen pieneksi kirjaimeksi. 

toLowerCase()-menetelmän toteutus perustuu ASCII-taulukkoon, jossa jokaisella merkillä on numeerinen arvo. Se hyödyntää eroa suuren ja pienen kirjaimen välisissä ASCII-arvoissa ja muuntaa suuret kirjaimet pieniksi.

## Katso myös:

Tarkista nämä linkit saadaksesi lisätietoja Arduino String -luokasta ja sen toiminnasta:
- [Arduino String](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [ASCII Kooditaulukko](http://www.asciitable.com/)
- [Arduino ToLowerCase - koodin läpikäynti](https://www.electronicwings.com/arduino/arduino-string-to-lowercase)