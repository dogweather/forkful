---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "PHP: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Luodessaan väliaikaisia tiedostoja, ohjelmoijat voivat käsitellä ja tallentaa väliaikaista tietoa ilman pysyvää muutosta alkuperäisiin tiedostoihin. Tämä on erityisen hyödyllistä esimerkiksi tietojen tallentamisessa järjestelmän toiminnan aikana tai muokatessa tiedostoja tilapäisesti.

## Kuinka tehdä:

```PHP
$tempfile = tmpfile();
fwrite($tempfile, "Tämä on väliaikainen tiedosto");
echo fread($tempfile, filesize($tempfile));
fclose($tempfile);
```

Tulos:
```
Tämä on väliaikainen tiedosto
```

## Syvällinen sukellus:

Luodessaan väliaikaisia tiedostoja PHP-koodilla, käytetään funktiota `tmpfile()`, joka luo tiedostonpalauttimen ja avaa sen tiedostoon. Tämän jälkeen tiedostoon voidaan kirjoittaa tai lukea kuten normaaleihin tiedostoihin. Tiedoston sulkemisen jälkeen se poistetaan automaattisesti, joten käyttäjän ei tarvitse huolehtia sen poistamisesta. Aiemmin PHP:ssä väliaikaiset tiedostot luotiin käyttämällä funktiota `fopen()` ja sen jälkeen poistamalla tiedosto manuaalisesti.

Vaihtoehtoisesti, jos väliaikaisen tiedoston luominen ei ole välttämätöntä, voidaan käyttää muuttujaa tai taulukkoa tietojen tallentamiseksi.

## Katso myös:

- PHP virallinen dokumentaatio `tmpfile()` funktiosta: https://www.php.net/manual/en/function.tmpfile.php
- Artikkeli "Temporary files in PHP" (englanniksi): https://www.php.net/manual/en/features.file-upload.php#114004
- PHP-keskustelufoorumi aiheesta: https://stackoverflow.com/questions/642125/managing-temporary-files-in-php