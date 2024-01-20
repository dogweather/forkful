---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Elm: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi? 
Tarkistaminen, onko tiedostokansio olemassa, on prosessi jolla selvitetään, onko tietty kohde olemassa tiedostojärjestelmässä. Tätä tarvitaan usein ohjelmointitehtävissä, jotta voidaan estää tiedostojen tai sovelluksen virheet, jotka johtuvat olemattoman kansion käsittelystä.

## Kuinka:
Elm ei tue suoraan tiedostojärjestelmän kansioita koskevien tietojen käsittelyä, koska se on suunniteltu puhtaaksi funktio-ohjelmointikieleksi, joka painottaa turvallisuutta ja ennustettavuutta, ja jossa sivuvaikutuksia tulisi välttää.

```Elm
-- Elm-koodi, joka näyttää perusoperaation
funktio = "Koodiesimerkki"
```
Koska Elm ei tue suoraan tiedostojärjestelmän operaatioita, kansioiden tarkistaminen on toteutettava itse. Tämä voi tapahtua esimerkiksi viestimällä JavaScript-koodin kanssa, jolla on kyky suorittaa tiedostojärjestelmän tehtäviä, tai käyttämällä palvelimen puolen koodia ja HTTP-pyyntöjä.

## Syvällä tietoa:
Menneisyydessä, kun ohjelmistot ja ohjelmointikielet olivat vähemmän kehittyneitä, tiedoston tai kansion olemassaolon tarkistaminen saattoi olla yksinkertainen operaatio. Nykyään, kuitenkin, tälläisten tietojen saaminen voi vaatia monimutkaisia menettelyjä, erityisesti sellaisissa turvallisissa ja puhtaissa ohjelmointikielissä, kuten Elm.

Tämä ei tarkoita, että on mahdotonta tarkistaa, onko kansio olemassa Elm-ohjelmassa. Elm tukee viestintää muiden ohjelmointikieleiden, kuten JavaScriptin, kanssa. Tämä viestintä voi toteutua esimerkiksi Elm:n komennon `port` avulla.

Vaihtoehtoisesti, voit käyttää HTTP-pyyntöjä tiedostojärjestelmä tietojen saamiseksi palvelimelta, jossa toimintoja ei ole rajoitettu samalla tavalla kuin Elm-koodissa.

## Katso myös:
- [Elm:n virallinen opas](https://guide.elm-lang.org/)
- [HTTP-pyynnöt Elm:ssä](https://guide.elm-lang.org/effects/http.html)