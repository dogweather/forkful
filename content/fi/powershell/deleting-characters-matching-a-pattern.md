---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "PowerShell: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Poistaessa merkkejä, jotka täyttävät tietyn mallin, tarkoitetaan tiettyjen merkkien poistamista tekstin osasta. Tämä on tärkeä tehtävä, kun halutaan puhdistaa tai muokata tekstiä tietystä mallista riippuen. Ohjelmoijat tekevät tätä usein, jotta heillä olisi käytössään puhdas ja järjestetty koodi.

## Kuinka se tehdään:
```PowerShell
# Esimerkki 1: Poista tietyn merkkijonon toiston
$teksti = "Tervetuloa tervetuloa! Tämä on esimerkki."
$puhdas_teksti = $teksti -creplace "(t|T)ervetuloa ", ""
echo $puhdas_teksti
# Output: Tämä on esimerkki.

# Esimerkki 2: Poista tietystä merkistä eteen tai taakse jäävät merkit
$teksti = "((1)2)3"
$puhdas_teksti = $teksti -creplace "\(|\)", ""
echo $puhdas_teksti
# Output: 123
```

## Syventävä katsaus:
Poistaminen merkkejä vastaavan mallin perusteella on yleinen tehtävä tekstikäsittelyssä. Sitä käytetään esimerkiksi tiedostojen käsittelyssä tai yksinkertaisesti tekstin muokkaamisessa. PowerShellin "-creplace" -komentoa voidaan käyttää poistamaan tietyn merkkijonon toistosta, kuten esimerkissä 1. Samaa komentoa voidaan käyttää myös poistamaan kaikki tietyt merkit rivistä, kuten esimerkki 2 osoittaa. On myös muita vaihtoehtoja, kuten "-replace" -komento tai Regular Expressions -kirjasto.

## Katso myös:
- [PowerShell -creplace -komento](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1#regex-based-operators)
- [PowerShell -replace -komento](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/replace?view=powershell-7.1)
- [Regular Expressions -kirjasto](https://www.regular-expressions.info/)