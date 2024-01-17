---
title:                "Merkkijonon ensimmäisen kirjaimen kirjoittaminen isoksi"
html_title:           "Arduino: Merkkijonon ensimmäisen kirjaimen kirjoittaminen isoksi"
simple_title:         "Merkkijonon ensimmäisen kirjaimen kirjoittaminen isoksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Miks
Angles
For people new to programming, one of the first things you may come across is the concept of "capitalizing a string". This simply means converting all the letters in a word or phrase to uppercase. Programmers do this for a variety of reasons, such as ensuring consistency in data or making comparisons easier.

Mikä & Miksi?
Miksi haluat muuttaa jonkin sanan tai lauseen isot kirjaimet? No, on monia syitä! Ehkä haluat varmistaa yhtenäisyyden datassa tai helpottaa vertailuja.

Miten:
Aloitetaan yksinkertaisesta koodiesimerkistä, joka muuttaa sanan "koodaus" isot kirjaimet "KOODAUS" käyttäen Arduino-ohjelmointikieltä:
```
Arduino

/* Aluksi, määritetään muuttuja "sana", johon tallennetaan haluttu sana*/
String sana = "koodaus";

/* Käytetään String-kirjastoa, joka sisältää capitalize() funktion */
String cappattu_sana = sana.capitalize();

/* Tulostetaan kapitalisoitu sana sarjamonitoriin  */
Serial.println(cappattu_sana);

/* Tämä tulostaa "KOODAUS" */
```
Yksinkertaisesti siis luodaan muuttuja, johon tallennetaan alkuperäinen sana ja käytetään siihen sisäänrakennettua capitalize () -funktiota, joka palauttaa saman sanan mutta isot kirjaimet. Sitten voimme tulostaa kapitalisoidun sanan sarjamonitoriin.

Deep Dive:
Capitalizing strings ei ole uusi käsite. Se on ollut osa ohjelmointia vuosikymmenten ajan. Alkuperäisesti se tehtiin jokaisen kirjaimen käsittelyllä, jossa pienet kirjaimet muutettiin isot ja tallennettiin uuteen muuttujaan. Nykyiset ohjelmointikielet, kuten Arduino, tarjoavat sisäänrakennetun toiminnon, joka tekee tämän prosessin helpommaksi.

On myös muita tapoja kapitalisoida merkkijonoja, kuten käyttämällä valmista funktiota tai kirjastoa, joka tekee sen automaattisesti. Esimerkiksi String libraryllä on myös toinen toiminto capitalizeWords(), joka muuttaa jokaisen sanan alkukirjaimen isoksi.

Katso myös:
- [C++ Reference - touppercase](https://www.cplusplus.com/reference/string/string/toupper/)
- [W3Schools - PHP strtoupper()](https://www.w3schools.com/php/func_string_strtoupper.asp)