---
title:                "Työskentely jsonin kanssa"
html_title:           "Bash: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
JSON (JavaScript Object Notation) on tiedostomuoto, joka mahdollistaa tietojen tallentamisen ja jakamisen jäsennellyssä muodossa. JSONia käytetään usein ohjelmoijien kesken esimerkiksi web-sovelluksien kehityksessä. Se on suosittu, sillä se on helppo lukea ja muokata, sekä helposti yhteensopiva muiden ohjelmistojen kanssa.

## Kuinka tehdään:
JSON-tiedosto koostuu avaimista ja niitä vastaavista arvoista. Avaimet ovat merkkijonoja ja arvot voivat olla esimerkiksi lukuja, merkkijonoja tai taulukoita. Tämän lisäksi JSON-tiedostoon kuuluu alku- ja loppumerkit "{ }". Koodiesimerkki näyttää, kuinka luodaan JSON-tiedosto Bashilla ja tulostetaan sen sisältö komentoriville.

```Bash
# Luodaan JSON-tiedosto
echo '{"nimi": "Maija Meikäläinen", "ikä": 30, "harrastukset": ["lukeminen", "lenkkeily"]}' > tiedosto.json

# Tulostetaan JSON-tiedoston sisältö
cat tiedosto.json
```
Tulostus näyttää seuraavalta:

```Bash
{"nimi": "Maija Meikäläinen", "ikä": 30, "harrastukset": ["lukeminen", "lenkkeily"]}
```

## Syväluotaus:
JSON luotiin alunperin Javascript-kielen yhteyteen, mutta siitä on tullut yleisesti käytetty tiedostomuoto myös muiden kielien, kuten Bashin, keskuudessa. Alternatiivisesti voit tallentaa tietoja myös esimerkiksi CSV- tai XML-tiedostoiksi, mutta JSON on usein selkeämpi ja helpompi käsitellä. Bashilla JSON-tietojen käsittelyyn on olemassa myös erilaisia työkaluja ja kirjastoja, jotka tarjoavat lisäominaisuuksia ja helpottavat työtä.

## Katso myös:
- [JSON:n virallinen sivusto](https://www.json.org/)
- [Bashin JSON-kirjasto jq](https://stedolan.github.io/jq/)
- [Ohjelmointiopas Bashilla työskentelyyn](https://csivola.net/bash/)