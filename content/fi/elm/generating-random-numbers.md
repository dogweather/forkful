---
title:                "Elm: Satunnaisten numeroiden generoiminen"
simple_title:         "Satunnaisten numeroiden generoiminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi käyttää satunnaislukugeneraattoria Elm-ohjelmoinnissa?

Satunnaislukugeneraattori on tärkeä työkalu monissa ohjelmointitehtävissä. Sen avulla voidaan luoda arvaamattomia lukuja, jotka ovat tarpeen esimerkiksi peleissä, simulaatioissa ja salausalgoritmeissa. Elm tarjoaa helpon tavan generoida satunnaislukuja, joten se on erinomainen valinta projekteihin, joissa satunnaislukujen käyttö on olennaista.

## Kuinka käyttää satunnaislukugeneraattoria Elm-ohjelmoinnissa?

Satunnaislukujen generointi Elm:ssa onnistuu käyttämällä `Random`-moduulia. Ensimmäiseksi on tuotava tämä moduuli ohjelmaan käyttämällä `import`-lauseketta. Sitten satunnaislukugeneraattori luodaan halutun tyypin mukaan, esimerkiksi `Float`-tyyppistä satunnaislukua varten. Lopuksi tulostamme generoidun luvun käyttämällä `seed`-funktiota ja määrittämällä sen parametriksi halutun luvun haarukan.

``` Elm
import Random
randomNumber : Float
randomNumber = 
  Random.generate 
    (Random.float 0 10) -- Haarukka välillä 0-10
    Random.initialSeed
```

Tulostus voi vaihdella jokaisella suorituskerralla, mutta seuraavassa on esimerkki mahdollisesta tuloksesta:

``` Elm
9.362957
```

## Syvällisempi tarkastelu satunnaislukugeneraattorista Elm:ssa

Elm:n `Random`-moduuli käyttää sisäisesti Mersenne Twister -algoritmia satunnaislukujen generointiin. Tämä algoritmi on suorituskykyinen ja tarjoaa hyvän satunnaisuuden. 

Satunnaislukugeneraattori tarvitsee myös alkuperäisen siemenen, joka sitten käytetään uusien lukujen luomiseen. On tärkeää huomata, että jos käytetään samaa siementä useammin, generoitu satunnaislukusarja on aina sama. Siksi onkin hyvä idea käyttää esimerkiksi ajanhetkestä tai käyttäjän antamasta syötteestä saatua siementä, jolloin taataan arvaamaton satunnaisuus.

## Katso myös

- [Elm:n virallinen dokumentaatio satunnaislukugeneraattorista](https://package.elm-lang.org/packages/elm/random/latest/)
- [Mersenne Twister -algoritmin tutkimuspaperi](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf)