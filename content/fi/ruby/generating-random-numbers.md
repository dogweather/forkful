---
title:                "Satunnaisten lukujen luominen"
html_title:           "Ruby: Satunnaisten lukujen luominen"
simple_title:         "Satunnaisten lukujen luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat luoda tai testata jotakin, jossa tarvitaan satunnaislukuja, Rubyn sisäänrakennettu satunnaislukugeneraattori voi olla erittäin hyödyllinen ja helppo tapa saavuttaa tämä tavoite.

## Miten

```Ruby
# Luodaan satunnaisluku välistä 1-10
puts rand(1..10) 
# Tulostaa esimerkiksi: 7

# Luodaan satunnaisluku väliltä 0-1
puts rand() 
# Tulostaa esimerkiksi: 0.478202347

# Luodaan joukko satunnaislukuja
puts Array.new(3) { rand(50..100) }
# Tulostaa esimerkiksi: [64, 87, 55]
```

## Syväluotaus

Ruby ohjelmointikielessä on sisäänrakennettu satunnaislukufunktio, joka käyttää Mersenne Twister -algoritmia. Tämä algoritmi on suosittu satunnaislukugeneraattori, joka tuottaa korkealaatuisia ja tasaisesti jakautuneita satunnaislukuja. Jos haluat tarkempaa kontrollia satunnaislukujen luomiseen, voit myös asettaa satunnaislukugeneraattorin siemenen ```srand()```-funktiolla.

## Katso myös

- [Ruby Docs - Random Class](https://ruby-doc.org/core-2.7.1/Random.html)
- [Mersenne Twister Algorithm Implementation in Ruby](https://github.com/seattlerb/mersenne_twister)
- [RubyMonk: Randomness in Ruby](https://rubymonk.com/learning/books/3-ruby-primer-ascent/chapters/11-randomness/lessons/47-randomness-in-ruby)