---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Satunnaisten Numeroiden Generointi Rubyssa

GENEROI RANDOM NUMEROITA RUBY-KIELELLÄ HELPOSTI!

## Mitä & Miksi?

Satunnaisten numeroiden generointi tarkoittaa numeroiden luomista, joilla ei ole mitään ennustettavaa järjestystä. Ohjelmoijat tarvitsevat niitä peleissä, salauksessa, algoritmien testauksessa ja monissa muissa käyttötapauksissa.

## Kuinka:

Satunnaisten numeroiden generointi Rubyssa on todella helppoa. Ruby tarjoaa `rand`-metodin juuri tätä varten.

```Ruby
puts rand(10) # Tulostaa satunnaisen numeron väliltä 0...10
```

Jos haluat generoida satunnaisia kokonaislukuja tietyltä väliltä, käytä `Random.new` yhdessä `rand`-metodin kanssa.

```Ruby
random_generator = Random.new
puts random_generator.rand(1..10) # Tulostaa satunnaisen numeron väliltä 1 – 10
```

## Syvällinen Tutkimus:

### Historiallinen Konteksti: 

Random numeroiden generointi on ollut tärkeä osa tietojenkäsittelytieteen historiaa. Alan pioneerit, kuten John von Neumann huomasi sen merkityksen algoritmin testauksissa.

### Vaihtoehdot: 

Erilaisia menetelmiä satunnaisten numeroiden generointiin on monia. Esimerkiksi Mersenne Twister tai Linear congruential generator (LCG) ovat populaarisia menetelmiä.

### Toteutuksen Yksityiskohdat: 

Ruby kääntää `rand`-funktion suoraan C-kielelle, joka tekee sen tehokkaaksi ja nopeaksi. Tarkemmin ottaen, se käyttää pseudorandom-numerogeneraattoria, nimeltään Mersenne Twister.

Rubyssa on myös SecureRandom-moduuli, joka tuottaa salatun satunnaislukujoukon. Tätä käytetään yleisemmin turvallisuuskriittisissä tapauksissa, kuten salauksessa.

```Ruby
require 'securerandom'
puts SecureRandom.hex(10) # Tulostaa 10 merkkiä pitkän satunnaisen heksadesimaalin.
```

## Katso Myös:

1. [Ruby-Dokumentaatio: Kernel.rand](https://ruby-doc.org/core-3.0.0/Random.html)
2. [Ruby-Dokumentaatio: SecureRandom](https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/SecureRandom.html)
3. [Wikipedia: Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
4. [Wikipedia: Linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)