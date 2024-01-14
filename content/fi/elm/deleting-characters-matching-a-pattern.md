---
title:    "Elm: Kaavan mukaisten merkkien poistaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Monet koodaajat saattavat joutua poistamaan merkkejä, jotka täsmäävät tiettyyn kuvioon. Tämä voi olla esimerkiksi tarpeen, kun halutaan käsitellä syötteitä tai tekstejä tietyllä tavalla. Elm tarjoaa helpon tavan suorittaa tämä toiminto koodissa, mikä voi säästää aikaa ja vaivaa.

## Miten se tehdään

Elmillä on käytössään sisäänrakennettu String-moduuli, joka tarjoaa erilaisia toimintoja merkkijonojen käsittelyyn. Yksi näistä toiminnoista on `filter`, joka poistaa halutun kuvion täsmäävät merkit merkkijonosta. Alla on yksinkertainen esimerkki tämän toiminnon käytöstä:

```Elm
inputString = "Hei, tässä on esimerkkiteksti!"
filteredString = String.filter (\c -> c /= ',') inputString
```
Tämä koodi poistaa kaikki pilkut merkkijonosta ja tuloksena on: `"Hei tässä on esimerkkiteksti!"`.

Voit myös käyttää regex-kirjastoa `elm/regex` suorittaaksesi tarkempaa kuvionmukaisen poiston. Tässä on esimerkki Regex-moduulin käytöstä:

```Elm
import Regex exposing (..)

inputString = "123-456-789"
pattern = Regex.regex "\\d"
filteredString = Regex.replace pattern (\_ match -> "") inputString
```
Tämä koodi poistaa kaikki numerot merkkijonosta ja tuloksena on tyhjä merkkijono `""`.

## Syvällinen sukellus

`filter`-toiminto käyttää takaa päin rekursion käsitellessään merkkijonoa. Tämä tarkoittaa, että se aloittaa merkkijonon lopusta ja poistaa merkit, jotka täsmäävät kuvioon kunnes se saavuttaa merkkijonon alun. Tämä lähestymistapa voi olla hyödyllinen, jos haluat poistaa merkkejä monimutkaisemmista osista merkkijonoa. Voit myös käyttää `foldl`-funktiota suorittaaksesi tämän toiminnon itse, mutta tämä vaatii hieman enemmän koodia.

## Katso myös

- Elm-käyttöohjeet: https://guide.elm-lang.org/
- Regex-moduulin dokumentaatio: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- String-moduulin dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/String