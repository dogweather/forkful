---
title:                "Työskentely yaml:n kanssa"
html_title:           "Elm: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi
Miksi henkilö haluaisi työskennellä YAML:n kanssa? YAML (Yet Another Markup Language) on ihanteellinen ratkaisu tietojen tallentamiseen ja luettavaksi muokkaustavaksi. Se on tehokas ja helppokäyttöinen formaatti, joka mahdollistaa rakenteellisesti jäsennetyn tiedon tallentamisen.

## Kuinka
YAML:in käyttöön aloittamiseksi täytyy ensin asentaa Elmin pakkaus ELm/Yaml. Tämän jälkeen, aloittaaksesi koodaamisen YAML:n kanssa, sinun täytyy tuoda se moduuliin:

```Elm
import Yaml exposing (..)
```
Tämän jälkeen voit käyttää muotoa ```decode```koodataksesi YAML:n datan 
joko arvoiksi tai JSON-blokeiksi. Esimerkiksi, seuraava YAML-data:

```Elm
name: "John Doe"
age: 30 
```
Muutetaan se Elm-tyyppiin:

```Elm
import Yaml.Decode as D

type alias Person = { name: String, age: Int }
let data = """
name: "John Doe"
age: 30 
"""
case D.decodeString D.$$ (D.map2 Person (D.index 1 D.string) <= D.index 2 D.int) data of
  Ok { name, age } -> -- do something with the decoded data
  Err e -> -- handle the error
```

Tässä tapauksessa ```D.$$``` toiminto ottaa vaaditut muunnokset YAML:dataan ja palauttaa ```Either String a```, mikä tarkoittaa joko Ok tai Error riippuen siitä, onko muuntaminen onnistunut vai ei. Käytännössä, ansaitseminen noilla D.$$ jotta noita ```index 1``` ja ```index 2``` data muuntaa stringiksi ja intiksi, ja muunnat sitten nuo lopuksi Henkilöobjektiksi.

## Syvemmälle

Elm/Yaml pohjimmiltaan lukee läpi YAML-dataa ja muuntaa sen dataksi, jota Elm voi käsitellä. Se on kuitenkin tärkeää huomata, että Elm/Yaml ei voi käsitellä kaikkia YAML:n ominaisuuksia. Se tukee vain perustoimintoja, kuten scalaareja, luetteloita ja avaimen/arvon pareja. Lisäksi, Elm/Yaml ei tue kaikkia YAML:n dataformaatteja, kuten esimerkiksi binäärijoukkoja.

Jos haluat tehdä muuntamisen monimutkaisemmille tyypeille, voit käyttää ```map``` ja ```andThen``` funktioita muokkaamaan dataa ennen sen muuntamista Elm-tyyppyksi.

Jotta pääset lisätietoihin muunnoksessa, suosittelemme tutkimaan Elm/Yaml dokumentaatiota ja koodiesimerkkejä GitHubista.

## Katso myös

- Elm/Yaml dokumentaatio: https://package.elm-lang.org/packages/chiroptical/elm-yaml/latest/
- Elm/Yaml esimerkit: https://github.com/chiroptical/elm-yaml/tree/master/example
- YAML:n dokumentaatio: https://yaml.org/spec/1.2/spec.html