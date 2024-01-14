---
title:                "Arduino: Muuntaa merkkijono pieniksi kirjaimiksi"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuntaa merkkijonoa pieniksi kirjaimiksi Arduino-ohjelmoinnissa? Muunnamalla merkkijono pieniksi kirjaimiksi, voit helpommin vertailla ja käsitellä tekstiä ohjelmassa. Esimerkiksi, jos haluat tarkistaa käyttäjän antaman syötteen, on helpompaa vertailla pienin kirjaimin kirjoitettua sanaa kuin tapausta herkkiä.

## Miten tehdä

Voit käyttää Arduino-ohjelmissa sisäänrakennettua toimintoa *toLowerCase()*. Se ottaa merkkijonon parametrinaan ja palauttaa muunnetun version.

```Arduino
String sana = "ARDUINO";
sana.toLowerCase(); // palauttaa "arduino"
```

## Syvempi sukellus

Merkkijonojen muuntaminen pieniksi kirjaimiksi suorittaa ASCII-taulukon avulla. ASCII-taulukko on joukko numeroita, jotka vastaavat eri merkkien kirjain- ja erikoismerkkejaksoja. Pienet kirjaimet ovat suurempien numeroiden alueella kuin isot kirjaimet. Tämän vuoksi palvelu *toLowerCase()* käy läpi koko merkkijonon ja vähentää jokaisen suuren kirjaimen numeron arvosta 32, jolloin se muuntuu pieneksi kirjaimeksi.

## Katso myös

- [ASCII-taulukko](https://www.asciitable.com)
- [Toinen esimerkki merkkijonon muuttamisesta pieniksi kirjaimiksi](https://roboticadiy.com/arduino-lowercase-uppercase-string-code/)