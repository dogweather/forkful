---
title:                "Kirjoittaminen standardivirheeseen"
html_title:           "Elm: Kirjoittaminen standardivirheeseen"
simple_title:         "Kirjoittaminen standardivirheeseen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Kirjoittaminen vakioerolle on tapa, jolla ohjelmoijat voivat ilmoittaa ohjelman suorituksessa tapahtuneista virheistä tai häiriöistä. Tämä auttaa kehittäjiä tunnistamaan ja korjaamaan ongelmia, mikä parantaa ohjelman toimintaa ja luotettavuutta.

## Miten:

```Elm
import Debug

Debug.crash "Tapahtui virhe"
```

Tämä koodiesimerkki käyttää `Debug`-moduulia kirjoittamaan vakioerolle viestin "Tapahtui virhe". Tämä viesti näkyy ohjelman suorittamisen yhteydessä henkilökohtaisessa konsolissasi tai kehittäjän välineissä, kuten Chrome DevToolsissa.

Vakioerolle kirjoittaminen on erityisen hyödyllistä, kun haluat ilmoittaa poikkeuksellisista tapahtumista tai virheistä, jotka eivät ehkä näy tavallisessa käyttäjälle näkyvässä käyttöliittymässä.

## Syvemmälle:

Vakioerolle kirjoittaminen on yleinen tapa käsitellä virheitä ja merkityksellinen myös muissa ohjelmointikielissä. Joissakin kielissä, kuten Java, vakioerolle kirjoittaminen tapahtuu automaattisesti, ja kehittäjän ei tarvitse erikseen kirjoittaa koodia virheiden ilmoittamiseksi.

On myös muita tapoja käsitellä virheitä, kuten `Result` ja `Maybe` tyyppitietojen käyttäminen Elm-kielessä. Nämä vaihtoehdot voivat olla parempia tietyissä tilanteissa, joten kannattaa tutustua niihin ennen kuin käytät vakioerolle kirjoittamista.

Vakioerolle kirjoittamisessa käytetään standardipuskurialuetta, joka on osa tietokoneen muistia, johon ohjelma voi kirjoittaa tietoja suorituksen aikana. Kun ohjelma kaatuu, tämä puskurialue näytetään kehittäjälle viestinä, joka auttaa tunnistamaan virheen syyn.

## Katso myös:

- [Elm Debug moduuli dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm Result ja Maybe tyyppitiedot](https://guide.elm-lang.org/error_handling/)