---
date: 2024-01-26 03:45:05.079263-07:00
description: "Kuinka: Elmin `Basics`-moduuli tarjoaa n\xE4pp\xE4ri\xE4 funktioita\
  \ py\xF6rist\xE4miseen: `round`, `floor` ja `ceiling`. N\xE4in niit\xE4 k\xE4ytet\xE4\
  \xE4n."
lastmod: '2024-03-13T22:44:56.483401-06:00'
model: gpt-4-0125-preview
summary: "Elmin `Basics`-moduuli tarjoaa n\xE4pp\xE4ri\xE4 funktioita py\xF6rist\xE4\
  miseen."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Elmin `Basics`-moduuli tarjoaa näppäriä funktioita pyöristämiseen: `round`, `floor` ja `ceiling`. Näin niitä käytetään.

```elm
import Basics exposing (round, floor, ceiling)

-- Pyöristä lähimpään kokonaislukuun
round 3.14    --> 3
round 3.5     --> 4

-- Pyöristä alas
floor 3.999   --> 3

-- Pyöristä ylös
ceiling 3.001 --> 4

-- Poista desimaalit pyöristämättä
truncate 3.76 --> 3
```

Elm tarjoaa myös `toLocaleString`-funktion pyöristämiseen kiinteään määrään desimaalipaikkoja:

```elm
import Float exposing (toLocaleString)

-- Pyöristä kahteen desimaalipaikkaan
toLocaleString 2 3.14159 --> "3.14"
```

## Syväsukellus
Elm on vahvasti tyypitetty funktionaalinen kieli, joka siirtää sivuvaikutukset arkkitehtuurin "reunoihin". Tämä tarkoittaa, että funktiot kuten pyöristäminen on oltava puhtaita ja ennustettavia. Historiallisesti pyöristäminen on yleinen toiminto monissa ohjelmointikielissä, jotka käsittelevät liukulukuaritmetiikan epätarkkuutta.

Elmin lähestymistapa pyöristämiseen on suoraviivainen - funktiot ovat puhtaita ja noudattavat matemaattisia määritelmiä pyöristämiseen, lattiaan ja kattoon. Elm ennakoi yleiset tarpeet tarjoamalla sisäänrakennettuja funktioita, sillä tarkkuuden hallinta on yleinen vaatimus, erityisesti rahoituksessa ja grafiikassa.

Vaihtoehtoja Elmin sisäänrakennetuille funktioille voisi sisältää mukautetut toteutukset käyttäen aritmeettisia operaatioita, mutta se lisäisi tarpeetonta monimutkaisuutta, kun standardikirjasto jo tekee työn tehokkaasti.

Nykyisessä versiossaan Elm käyttää JavaScriptin taustalla olevaa liukulukumatematiikkaa näille operaatioille, pysyen siten yhdenmukaisena IEEE 754 -standardin kanssa, mikä on hyvä muistaa, kun pohditaan tarkkuutta ja mahdollisia liukulukuvirheitä.

## Katso Myös
- Elmin virallinen `Basics`-moduulin dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Yksityiskohtainen tarkastelu siitä, miten liukuluvut toimivat tietotekniikassa: https://floating-point-gui.de/
- Elm `Float`-moduuli lisää liukulukuoperaatioihin: https://package.elm-lang.org/packages/elm/core/latest/Float
