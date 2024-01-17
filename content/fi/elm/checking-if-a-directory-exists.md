---
title:                "Tarkistaako hakemiston olemassaolon"
html_title:           "Elm: Tarkistaako hakemiston olemassaolon"
simple_title:         "Tarkistaako hakemiston olemassaolon"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tarkistaa, onko hakemistoa olemassa, on yksinkertaisesti tarkistus, joka auttaa ohjelmoijia varmistamaan, että tarvitseva hakemisto on olemassa ennen kuin tehdään siihen liittyviä toimintoja. Tämä voi auttaa välttämään virheitä tai häiriöitä ohjelman suorittamisessa.

## Miten:
Elmissa hakemistojen tarkistaminen tapahtuu käyttämällä puun hakemistoista vastaavaa moduulia. Tämä moduuli tarjoaa funktion, joka ottaa parametriksi hakemiston polun ja palauttaa Bool-tyypin arvon, joka kertoo onko hakemisto olemassa vai ei. Alla on esimerkki koodista:

```Elm
import Directory exposing (exists)

hakemistoPolku = "/kansio1/kansio2"

onkoHakemistoOlemassa = exists hakemistoPolku

case onkoHakemistoOlemassa of
    True ->
        "Hakemisto on olemassa."
        
    False ->
        "Hakemistoa ei löydy."
```
Esimerkkikoodin output voi olla esimerkiksi "Hakemisto on olemassa.", jos hakemistoPolku vastaa olemassa olevaa hakemistoa, tai "Hakemistoa ei löydy.", jos hakemistoPolku vastaa hakemistoa, jota ei ole olemassa.

## Syvemmälle:
Hakemistojen tarkistaminen on tärkeä osa ohjelmien toimintaa, sillä usein tarvitaan tietoa siitä, onko hakemisto olemassa ennen kuin suoritetaan siihen liittyviä toimintoja. Ennen Elm-versiota 0.19, hakemistojen tarkistaminen tapahtui File moduulin avulla. Elm 0.19 toi kuitenkin mukanaan puun hakemistoista vastaavan moduulin, joka helpottaa hakemistojen tarkistamista ja tarjoaa selkeämmän käyttöliittymän.

## Katso myös:
- [Elm Puun hakemistot](https://package.elm-lang.org/packages/elm/directory/latest/Directory)
- [Elm File moduuli](https://package.elm-lang.org/packages/elm/file/latest/File)