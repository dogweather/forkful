---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen generointi tarkoittaa satunnaisten arvojen tuottamista tietokoneohjelmointiympäristössä. Ohjelmoijat tarvitsevat niitä simulaatioihin, testaukseen, pelien luomiseen ja salaustarkoituksiin.

## Miten:

Elmissa satunnaislukujen luominen tapahtuu Random -moduulin avulla. Tässä yksinkertainen esimerkki:

```Elm
import Random

arvoNoppaa : Random.Generator Int
arvoNoppaa =
  Random.int 1 6

main =
  Random.generate identity arvoNoppaa
```

Suorittaessasi tämän Elk-ohjelman, konsoliin tulostuu satunnainen luku väliltä 1-6.

## Syvempi sukellus:

Satunnaislukujen generoinnin historia ulottuu 1940-luvulle, jolloin ensimmäiset digitaaliset tietokoneet kehitettiin. Elmissä satunnaislukuja generoidaan pseudosatunnaisesti, joka tarkoittaa, että ne voivat näyttää satunnaisilta, mutta ne seuraavat itse asiassa ennustettavaa kaavaa.

Vaihtoehtoja:

- Elm tarjoaa myös `Random.float`-funktion, jolla voidaan tuottaa liukulukuja.

Yksityiskohdat:

- Elm-kielellä satunnaislukujen luonti vaatii random-seedit. Niitä ei voi luoda puhtaassa funktionaalisessa ympäristössä, joten Elm antaa sinun luoda seedit käyttämällä `Random.initialSeed`.

## Katso myös:

- Elm-ohjelmointikielen [Random -moduulin dokumentaatio](http://package.elm-lang.org/packages/elm-lang/core/5.0.0/Random)
- [Ymmärrä, kuinka tietokoneet tuottavat satunnaislukuja](https://en.wikipedia.org/wiki/Random_number_generation)
- [Pseudosatunnaislukugeneraattoreiden historia ja tekniikat](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)