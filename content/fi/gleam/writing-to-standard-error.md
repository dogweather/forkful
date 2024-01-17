---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Gleam: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kirjoittaminen standardivirheeseen on menetelmä, joka mahdollistaa virheilmoitusten lähettämisen suoraan konsolille. Tämä on tärkeää kehittäjille, koska se auttaa tunnistamaan ja korjaamaan ohjelmistoihin liittyviä ongelmia nopeammin ja tehokkaammin.

## Näin teet sen:

#### Kirjoittaminen standardivirheeseen:
```Gleam
io.println(os.Stderr, "Virheilmoitus")
```

#### Tuloste konsolilla:
```Gleam
Virheilmoitus
```

## Syvempi sukellus:

#### Historiallinen konteksti:

Standardivirheeseen kirjoittaminen on peräisin Unix-käyttöjärjestelmästä ja se on yleistynyt myös muissa käyttöjärjestelmissä. Tämä menetelmä on luotettava tapa käsitellä virheitä, ja se on vakiintunut osa ohjelmistojen kehitystä.

#### Vaihtoehtoiset menetelmät:

Vaikka standardivirheeseen kirjoittaminen on yleisesti hyväksytty menetelmä, on olemassa myös muita tapoja käsitellä virheilmoituksia. Esimerkiksi kirjoittaminen lokiin tai käyttämällä palvelinta tiedon tallentamiseen voivat olla vaihtoehtoisia lähestymistapoja.

#### Toteutusyksityiskohdat:

Kirjoittaminen standardivirheeseen tapahtuu käyttämällä ```io.println``` -funktiota Gleamissa. Tämä toimii samalla tavalla kuin tulostaminen konsolille normaalissa ```print``` -funktiossa, mutta käyttää neljättä parametria (```os.Stderr```) standardivirheen osoittamiseen.

## Katso myös:

- Gleamin viralliset ohjeet: https://gleam.run/book/tour/standard-error
- Unix-standardi: http://pubs.opengroup.org/onlinepubs/9699919799/functions/stderr.html