---
title:                "Ruby: Satunnaislukujen luominen"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi luoda satunnaisia numeroita ohjelmoinnissa? Satunnaiset numerot ovat hyödyllisiä monissa eri tilanteissa, kuten arpajaisissa, peleissä ja tietokonealgoritmeissa. Niitä voidaan myös käyttää testaamisessa ja simulaatioissa luomaan erilaisia skenaarioita. Ruby tarjoaa helpon tavan luoda satunnaisia numeroita ohjelmien tarpeisiin.

## Miten

Käytä `rand`-metodia luodaksesi satunnaisen kokonaisluvun välillä 0 ja 10:

```Ruby
rand(11)
```

Tämä palauttaa satunnaisen numeron väliltä 0-10, mukaan lukien 0 ja 10. Voit myös käyttää `rand`-metodia luodaksesi satunnaisen desimaaliluvun väliltä 0 ja 1:

```Ruby
rand()
```

Voit asettaa myös ylärajan `rand`-metodin argumentilla:

```Ruby
rand(1..100) # palauttaa satunnaisen numeron väliltä 1-100, mukaan lukien 1 ja 100
```

Voit myös käyttää `Random`-luokkaa, joka tarjoaa enemmän vaihtoehtoja satunnaisten numeroiden generoimiseen:

```Ruby
random = Random.new # Luo uuden Random-olion
random.rand(50) # palauttaa satunnaisen numeron väliltä 0-50
random.rand(1.5..2.5) # palauttaa satunnaisen desimaaliluvun väliltä 1.5-2.5
```

## Syvempää tietoa

Ruby käyttää satunnaisen numeron generoimiseen Mersenne Twister -algoritmia, joka voi tuottaa jopa 2^19937-1 erilaista satunnaista lukusarjaa. Tämä tekee siitä erittäin tehokkaan ja luotettavan tavan generoida satunnaisia numeroita ohjelmoinnissa.

On myös tärkeää muistaa, että nämä numerot eivät ole täysin satunnaisia, vaan ne perustuvat ennustettavaan algoritmiin. Tämä tarkoittaa, että jos sinulla on tarpeeksi tietoa ja aikaa, voit ennustaa seuraavan satunnaisen numeron.

## Katso myös

- [Ruby:Satunnaiset numerot](https://ruby-doc.org/core-2.6/Random.html)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)