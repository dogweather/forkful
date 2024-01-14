---
title:    "Ruby: Satunnaislukujen luominen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi
Miksi haluaisit käyttää satunnaisnumeroiden generointia ohjelmoinnissa? Satunnaiset numerot ovat hyödyllisiä monissa sovelluksissa, kuten pelien luonnissa, testauksessa ja salausavainten luomisessa.

## Miten
Satunnaisnumeroiden generointi Rubyssa on helppoa! Käytä vain `rand` metodia ja anna sille haluamasi numeroiden määrä. Esimerkiksi, jos haluat generoida 5 satunnaista kokonaislukua välillä 1-10, kirjoita seuraava koodi:

```Ruby
5.times do
  puts rand(1..10)
end
```

Tämä tulostaa jotakin seuraavista:

```
8
4
2
9
1
```

Voit myös käyttää `rand` metodia generoimaan satunnaisia desimaalilukuja. Voit tehdä tämän antamalla parametreiksi halutun minimi- ja maksimiarvon, esimerkiksi `puts rand(5.0..10.0)` tulostaa satunnaisen desimaaliluvun väliltä 5.0 - 10.0.

## Syväsukellus
Ruby käyttää Mersenne Twister algoritmia satunnaisien numeroiden generoimiseen. Tämän algoritmin avulla satunnaiset numerot ovat riippumatonta ajasta, joten jokainen generointi antaa täysin satunnaisen numeron. Voit lukea lisää tästä algoritmista ja sen taustoista [täältä] (https://ruby-doc.org/core-2.6.3/Random.html).

## Katso myös
- [Ruby:n satunnaiset numerot dokumentaatiossa] (https://ruby-doc.org/core-2.6.3/Random.html)
- [Mersenne Twister algoritmin selitys] (https://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html)
- [Satunnaisnumeroiden generointi käytännössä Rubylla] (https://www.rubyguides.com/2018/10/random-numbers-ruby/)