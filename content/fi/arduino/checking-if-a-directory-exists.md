---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Arduino: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tarkistamme onko hakemisto olemassa yksinkertaisesti vahvistaaksemme sen olemassaolo sijainnissa ennen tiedostojen luku- tai kirjoitustoimintoja. Jos programmia ei varmisteta ja yrität käyttää olematonta kansiota, ohjelma saattaa kaatua tai tuottaa virheen.

## Kuinka:

```Arduino
if (SD.exists("/example")) {
  Serial.println("Directory exists.");
} else {
  Serial.println("Directory does not exist.");
}
```

Esimerkissä ylhäällä tarkistamme, onko hakemisto nimeltä 'example' olemassa SD-kortissa. Jos se on olemassa, tulostetaan "Directory exists.". Jos ei, tulostetaan "Directory does not exist."

## Syväsukellus:

Hakemiston olemassaolon tarkistaminen ei ole uusi käsite; se on osa tiedostojärjestelmän hallintaa, joka on sisällytetty useimpiin ohjelmointikieliin. Arduinoon se tuli käyttöön, kun SD-kirjasto esiteltiin, mikä mahdollisti SD-kortin käytön datan tallentamiseen.

Vaihtoehtoisesti voit myös yrittää luoda hakemiston ja tarkistaa palautusarvon - jos luominen epäonnistuu, se tarkoittaa todennäköisesti, että hakemisto on jo olemassa.

On huomattava, että SD-kirjaston `exists`-metodi toimii sekä tiedostoille että hakemistoille. Tämä voi joskus aiheuttaa sekaannusta, jos tiedostolla on sama nimi kuin tarkistettavalla hakemistolla.

## Katso myös:

- Arduinon SD-kirjaston viralliset dokumentit: https://www.arduino.cc/en/reference/SD
- Vastaava StackOverflow-keskustelu: https://stackoverflow.com/questions/13546778/how-do-i-check-if-a-directory-exists-on-an-sd-card
- Tiedostojärjestelmien yleiset käsitteet: https://en.wikipedia.org/wiki/File_system