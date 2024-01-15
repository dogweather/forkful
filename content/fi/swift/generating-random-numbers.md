---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Swift: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit luoda satunnaisia numeroita? Monet ohjelmistokehittäjät käyttävät satunnaisia numeroita erilaisissa sovelluksissaan. Esimerkiksi pelien luomiseen, testien suorittamiseen ja salasanojen generoimiseen tarvitaan usein satunnaisia numeroita.

## Miten

Onneksi Swift tarjoaa helpon tavan luoda satunnaisia numeroita. Voit käyttää `arc4random_uniform()` -funktiota generoimaan satunnaisia numeroita halutulta väliltä. Katso alla oleva koodiesimerkki:

```Swift
let randomNumber = arc4random_uniform(10) //generoi satunnaisen numeron väliltä 0-9
print(randomNumber) //tulostaa satunnaisen numeron
```
Tämä koodi esimerkki generoi ja tulostaa satunnaisen numeron väliltä 0-9. Vaihtamalla funktion parameteria voit generoida satunnaisia numeroita eri väliltä.

## Syvällinen tarkastelu

Mikä onkaan sitten `arc4random_uniform()` -funktio? Se on Swiftin tarjoama funktio, joka käyttää algoritmia, joka pohjautuu Xorshift-generaattoriin. Tämä algoritmi takaa tasaisen jakauman eri numeroiden välillä ja antaa korkean satunnaisuuden tason.

On myös tärkeää huomata, että `arc4random_uniform()` -funktio generoi vain kokonaislukuja. Jos haluat generoida satunnaisia desimaalilukuja, voit käyttää `Double` tai `Float` tyyppisiä muuttujia.

## Katso myös

- [Swiftin virallinen dokumentaatio satunnaisista numeroista](https://developer.apple.com/documentation/swift/2920723-arc4random_uniform)
- [Ohjelmoijan Työkalupakki - Opas satunnaisien lukujen käyttöön Swiftissä](https://www.ohjelmointi.com/artikkelit/opas-satunnaislukujen-kayttoon-swiftissa/)
- [Swiftin kymmenen tärkeää funktiota](https://medium.com/flawless-app-stories/swift-functions-you-should-know-2b474af67a8f)