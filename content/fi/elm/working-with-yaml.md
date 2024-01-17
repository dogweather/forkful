---
title:                "Yhteistyössä yaml:n kanssa."
html_title:           "Elm: Yhteistyössä yaml:n kanssa."
simple_title:         "Yhteistyössä yaml:n kanssa."
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Kuulutko sinäkin niihin ohjelmoijiin, jotka ovat kuulleet puhuttavan YAMLista ja ihmettelevät, mistä oikein on kyse? Ei hätää, kerromme lyhyesti mistä on kyse ja miksi ohjelmoijat työskentelevät sen parissa.

YAML on tiedoston muoto, jolla tietoa voidaan tallentaa ja jakaa helposti luettavassa ja yksinkertaisessa muodossa. Se on erityisen kätevä esimerkiksi sovellusten ja tietokantojen asetuksien tallentamiseen. Ohjelmoijat käyttävät YAMLia helpottamaan datan käsittelyä ja tietojen siirtämistä eri ohjelmien välillä.

## Kuinka tehdä?

Elm-koodilohkoihin sisällytetyillä esimerkeillä näytämme, kuinka YAMLia käytetään Elm-ohjelmoinnissa.

```Elm
-- Tuodaan YAML-paketti käyttöön
import YAML exposing (..)

-- Luodaan esimerkki YAML-tiedostosta
example =
  """
  Otsikko: Tervetuloa!
  Viesti: Tässä on esimerkki YAML-tiedostosta.
  Lista:
    - Yksi
    - Kaksi
    - Kolme
  """

-- Parsitaan YAML-tiedosto muuttujaksi
result = parse example

-- Tulostetaan tietoja konsoliin
case result of
  Ok yaml ->
    -- Tulostaa otsikon
    log (yaml["Otsikko"])
    -- Tulostaa viestin
    log (yaml["Viesti"])
    -- Tulostaa listan ensimmäisen arvon
    log (yaml["Lista"][0])
  Err error ->
    log error
```

Tulostus konsoliin:

```
Tervetuloa!
Tässä on esimerkki YAML-tiedostosta.
Yksi
```

## Syväsukellus

YAML kehitettiin vuonna 2001 tavoitteena olla yksinkertainen ja helppokäyttöinen formaatti eri ohjelmointikielten välillä. Sen vaihtoehtoina voidaan käyttää esimerkiksi XML- ja JSON-formaatteja, mutta YAML eroaa niistä siinä, että se ei vaadi sulkuja ja pisteitä merkkijonojen ympärille.

YAML-ohjelmointikieliliittymä on saatavilla useimpiin ohjelmointikieliin ja sen toteutus perustuu tiedostopohjaiseen lähestymistapaan. Elm-kielellä YAMLia käytetään helposti sisällyttämällä YAML-paketti projektiin.

## Katso myös

- YAML-virallinen dokumentaatio: https://yaml.org/
- Elm YAML-paketti: https://package.elm-lang.org/packages/janiczek/elm-yaml/latest/