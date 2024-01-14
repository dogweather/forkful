---
title:    "Arduino: Tarkistetaan löytyykö hakemistoa"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi
Kun ohjelmoit Arduinoa, saatat joutua tarkistamaan, onko hakemistoa olemassa. Tämä on tärkeää, jotta voit varmistaa, että ohjelma toimii oikein ja käsittelee tiedostoja oikein.

## Miten
Voit tarkistaa hakemiston olemassaolon käyttämällä "File" -kirjastoa ja "exists" -funktiota. Tässä on esimerkki koodista ja tulosteesta:

```
Arduino #include <File.h>
File hakemisto = File("/tiedostot/kuvat");
if (hakemisto.exists()) {
  Serial.println("Hakemisto on olemassa!");
} else {
  Serial.println("Hakemistoa ei löytynyt.");
}
```

Tuloste:

```
Hakemisto on olemassa!
```

## Syvemmälle
Tarkistamalla hakemiston olemassaoloa, voit myös käyttää muita "File" -kirjaston funktioita, kuten "isDirectory()". Tämä tarkistaa, onko kyseessä todellinen hakemisto vai vain tiedosto.

```
Arduino #include <File.h>
File hakemisto = File("/tiedostot/kuvat");
if (hakemisto.isDirectory()) {
  Serial.println("Hakemisto löytyi!");
} else {
  Serial.println("Tämä ei ole hakemisto.");
}
```

Tuloste:

```
Hakemisto löytyi!
```

On myös tärkeää huomata, että voit tarkistaa hakemiston olemassaolon myös ennen sen avaamista kirjoittamalla "File.isValid()" -funktion. Tämä auttaa varmistamaan, että ohjelma ei kaadu, jos hakemistoa ei löydy.

## Katso myös
- "File" -kirjaston dokumentaatio (https://www.arduino.cc/reference/en/libraries/filesystem/)
- "exists" -funktion dokumentaatio (https://www.arduino.cc/reference/en/libraries/filesystem/exists/)
- "isDirectory" -funktion dokumentaatio (https://www.arduino.cc/reference/en/libraries/filesystem/isdirectory/)