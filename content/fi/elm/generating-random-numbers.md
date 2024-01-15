---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Elm: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaislukujen luominen on tärkeä osa ohjelmointia monissa sovelluksissa. Ne voivat auttaa luomaan monipuolisia toimintoja, kuten arpajaisia, salasanoja ja satunnaisia tietokannan viittauksia. Elm tarjoaa tehokkaan ja luotettavan tavan generoida satunnaisia lukuja, joten saatat haluta oppia lisää tästä tärkeästä taidosta.

## Miten

Satunnaislukujen luominen Elm:llä on helppoa. Voit käyttää `Random` kirjastoa, joka tarjoaa useita toimintoja satunnaisten lukujen generoimiseen. Tässä on yksinkertainen esimerkki, jossa generoidaan luku väliltä 1-10:

```Elm
import Random exposing (int, step)

generateRandomNumber : Random.Generator Int
generateRandomNumber =
  Random.step (Random.int 1 10)

main =
  Random.generate generateRandomNumber
```

Tässä koodin esimerkissä `Random` kirjastosta tuodaan `int` ja `step` toiminnot. `step` toiminto ottaa parametrina `Random.Generator` tyypin ja `Random.int` luo satunnaisen luvun annetulta väliltä. Lopuksi `Random` kirjasto käyttää `generate` toimintoa, joka käyttää `generateRandomNumber` funktiota ja luo satunnaisen luvun.

Suoritettaessa tätä koodia, voit odottaa tuloksena jotakin seuraavan kaltaista:

```
Ok 7 : Result String Int
```

Tämä tarkoittaa, että satunnainen luku on luotu onnistuneesti ja sen arvo on 7.

## Syvällisempi sukellus

Satunnaislukujen generoiminen Elm:llä perustuu `Random` kirjastoon ja sen toimintoihin. Voit käyttää esimerkiksi `float` toimintoa, jos haluat generoida satunnaisia liukulukuja. `int` ja `float` toiminnot hyväksyvät myös `min` ja `max` parametrit, joiden avulla voit määrittää halutun välillä.

Voit myös käyttää `stepWith` toimintoa, joka ottaa parametreina funktion ja `Random.Generator` tyypin. Tämä mahdollistaa monimutkaisempien satunnaislukujen generoinnin, kuten esimerkiksi luvun pohjalta toisen luvun luonti tai tiettyjen ehtojen täyttymisen tarkistaminen ennen luvun generointia.

Olennaista on kuitenkin pitää mielessä, että satunnaisluvut eivät ole täysin "satunnaisia" tietokoneessa, vaan niitä generoidaan tietynlaisen kaavan avulla. Tästä syystä niitä ei tulisi käyttää tietoturvasovelluksissa tai missään muussa tilanteessa, jossa todellinen satunnaisuus on välttämätön.

## Katso myös

- OfBorg satunnaislukugeneraattori Elm-ympäristöön: https://github.com/elm-community/elm-random-extra
- Elm Random kirjaston dokumentaatio: https://package.elm-lang.org/packages/elm/random/latest/Random