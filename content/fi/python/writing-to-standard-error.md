---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Python: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kirjoittaminen standardi virheeseen (standard error) on tapa, jolla ohjelmoijat voivat näyttää virheviestejä ohjelmastaan. Tämä auttaa ohjelmoijia löytämään ja korjaamaan ohjelmansa virheitä nopeammin.

## Kuinka:
`` ` Python
import sys
sys.stderr.write("Tämä on virheviesti!")
`` `
Esimerkkilähtö:
`Tämä on virheviesti!`

## Syventävä syventyminen:
Kirjoittaminen standardi virheeseen on keksitty osaksi Unix käyttöjärjestelmää, ja on nykyään yleisesti käytetty tapa näyttää virheviestejä. Vaihtoehtoisia tapoja ovat esimerkiksi tulostaminen standardiin tulosvirtaan tai luokkien käyttäminen virheiden käsittelemiseen. Implementation yksityiskohdat vaihtelevat eri ohjelmointikielissä.

## Katso myös:
[Python virheiden käsittely](https://docs.python.org/3/tutorial/errors.html)