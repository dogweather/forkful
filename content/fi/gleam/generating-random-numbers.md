---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen generoiminen tarkoittaa uniikkien, ennakoimattomien lukujen tuottamista. Ohjelmoijat tekevät tämän luodakseen satunnaista dataa tai simuloimaan satunnaisia tapahtumia.

## Kuinka:

Alla on esimerkkikoodi ja tuloste Gleamilla:

```gleam
import gleam/should
import gleam/random.{generator, int}

fn test() {
  let gen = generator.new_seed()
  let (gen, number) = int(1, 6, gen)
  
  should.equal(should.be_in([1, 2, 3, 4, 5, 6], number), Ok(Nil))
}
```

Tässä koodissa luodaan uusi satunnaislukugeneraattori, jonka jälkeen tuotamme satunnaisen kokonaisluvun. Sitten testaamme, että tuotettu luku kuuluu annettuun väliin.

## Syvä sukellus

1. Historiallinen konteksti: 1900-luvulla, tietokoneiden alkuaikoina, oikeiden satunnaislukujen tuottaminen oli haastavaa. Nykyisin ohjelmoijille on tarjolla monimutkaisia satunnaislukualgoritmeja, joita voidaan käyttää turvallisesti.
2. Vaihtoehdot: Valinnanvaraa on paljon. Esimerkiksi SecureRandom tuottaa satunnaislukuja, jotka ovat turvallisia kryptografisiin tarkoituksiin.
3. Toteutuksen yksityiskohdat: Gleam käyttää Erlangin `rand`-moduulia satunnaislukujen luomiseen. Se käyttää melko monimutkaista algoritmia, mutta tarjoaa hyvän tasapainon suorituskyvyn ja luotettavuuden välillä.

## Katso myös

1. Erlang `rand`-moduuli: [linkki](http://erlang.org/doc/man/rand.html)

---