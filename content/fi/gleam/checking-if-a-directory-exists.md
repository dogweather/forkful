---
title:                "Gleam: Tarkistetaan löytyykö kansio"
simple_title:         "Tarkistetaan löytyykö kansio"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa onko kansio olemassa?

Eräs yleinen tehtävä ohjelmoinnissa on tarkistaa, löytyykö tietyltä polulta tiettyä kansiota tai tiedostoa. Tämä voi olla tarpeen esimerkiksi ennen tiedoston avaamista tai tallentamista, jotta vältetään mahdolliset virheet. Gleam kielessä tämän voi tehdä kätevästi ja tehokkaasti Directory moduulilla.

## Miten tehdä se?

Gleamissa on valmiiksi integroitu `Directory.exists` funktio, joka palauttaa boolean arvon sen mukaan, löytyykö annetulta polulta kansiota tai tiedostoa. Tässä on yksinkertainen esimerkki:

```Gleam
import Directory

let directory_path = "./testikansio"

if Directory.exists(directory_path) {
  // Tee jotain jos kansio on olemassa
  IO.println("Kansio löytyi!")
} else {
  // Tee jotain jos kansioa ei ole olemassa
  IO.println("Kansiota ei löytynyt!")
}
```

Jos kansio löytyy, tulostetaan "Kansio löytyi!" ja muuten "Kansiota ei löytynyt!". Koodilohkon alussa tuodaan Directory moduuli ja sen jälkeen funktiota hyödynnetään if-lauseessa.

## Syvällisempi tutustumien kansiotarkistamiseen

Gleamin Directory moduli tarjoaa myös muita hyödyllisiä funktioita, kuten `Directory.list_files` ja `Directory.delete`. On myös mahdollista käyttää `Directory.exists` funktiota apuna esimerkiksi sellaisten polkujen luomisessa, jotka eivät saa johtaa tiedostoon tai kansion.

Tämän tarkempiin yksityiskohtiin pääsee käsiksi tutustumalla Gleamin viralliseen dokumentaatioon. Sieltä löytyy tarkemmat kuvaukset ja esimerkit jokaisesta Directory moduulin tarjoamasta funktiosta.

## Katso myös

- [Gleam Documentation - Directory](https://gleam.run/documentation/stdlib/Directory.html)
- [How to Check if a File or Directory Exists in Gleam](https://gleam.run/documentation/cookbook/check-if-file-directory-exists.html)