---
title:                "Arduino: Alustahakemiston olemassaolon tarkistus"
simple_title:         "Alustahakemiston olemassaolon tarkistus"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Miksi Arduino-ohjelmoijan kannattaa tarkistaa, onko kansio olemassa? Tarkistamalla kansion olemassaolon voit varmistaa, että ohjelmasi toimii halutulla tavalla ja välttää mahdollisia virheitä voidaan havaita ja korjata ennen kuin ne aiheuttavat suurempia ongelmia.

## Kuinka

Kansiot voidaan tarkistaa Arduino IDE:lla käyttämällä `SD.exists()` -komennon. Tämä komento palauttaa `true` tai `false` arvon, riippuen siitä, onko kansio olemassa vai ei. Esimerkiksi:

```Arduino
if (SD.exists("/kansio/")) {
  Serial.println("Kansio on olemassa!");
} else {
  Serial.println("Kansiota ei ole olemassa!");
}
```

Kun ohjelma ajetaan, se tulostaa joko "Kansio on olemassa!" tai "Kansiota ei ole olemassa!" riippuen kansion olemassaolosta.

## Syvempi sukellus

Kun tarkistat kansion olemassaolon, on tärkeää pitää mielessä, että `SD.exists()` -komennon tarkistama polku on suhteellinen polku. Tämä tarkoittaa, että se vertailee polkua Arduino-mikrokontrollerin sijaintiin eikä tietokoneesi kansioihin. Siksi on tärkeää varmistaa, että kansion sijainti osoitetaan oikein.

## Katso myös

- [Arduino-tietokannan ohjelmointi](https://www.arduino.cc/reference/en/libraries/sd/)
- [SD-biblioteca Arduino-oppaat](https://www.arduino.cc/en/Reference/SD)
- [Arduino tiedostojen hallinta](https://create.arduino.cc/projecthub/Arduino_Genuino/file-management-with-arduino-04f078)