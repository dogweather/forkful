---
title:                "Tarkistetaan tiedostohakemiston olemassaolo"
html_title:           "PHP: Tarkistetaan tiedostohakemiston olemassaolo"
simple_title:         "Tarkistetaan tiedostohakemiston olemassaolo"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Mitä ja miksi?
Tarkista, onko hakemisto olemassa, on yksinkertainen tapa tarkistaa, onko tietty hakemisto olemassa tietokoneella. Tämä on tärkeää ohjelmoijille, kun he haluavat varmistaa, että heidän koodi toimii oikein ja että edellytykset ovat olemassa ennen kuin ohjelma suoritetaan.

Kuinka:
```PHP
if (file_exists($hakemiston_nimi)) {
  echo "Hakemisto on olemassa.";
} else {
  echo "Hakemistoa ei löydy.";
}
```

Tulostus:
```
Hakemisto on olemassa.
```

Syvyys sukellus:
Aikaisemmin tietokoneissa ei ollut tapaa tarkistaa hakemistojen olemassaoloa. Tähän oli kehitettävä lisäämällä erityisiä tarkistuksia, jotka kävivät läpi tiedostojärjestelmän hakemistopolun ja palauttivat arvon, jos hakemisto ei ollut olemassa. Tänä päivänä tätä tarkistusta varten on kuitenkin valmis toiminto ```file_exists```, joka helpottaa ohjelmoijien elämää.

Tärkeä huomioitava seikka on, että ```file_exists``` palauttaa arvon myös, jos osoittimen osoittama tiedosto on olemassa. Tämä voi aiheuttaa ongelmia, jos ohjelmoija ei ole huolellinen. Tässä tapauksessa voi olla parempi käyttää ```is_dir``` -toimintoa, joka palauttaa totuusarvon vain, jos hakemisto on olemassa.

Katso myös:
https://www.php.net/file_exists
https://www.php.net/is_dir