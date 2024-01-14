---
title:                "Ruby: Satunnaisten lukujen generointi"
simple_title:         "Satunnaisten lukujen generointi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi käyttää satunnaislukugeneraattoria Ruby-ohjelmoinnissa? Satunnaislukujen luominen on tärkeä osa monia ohjelmointitehtäviä, kuten pelien kehittämistä tai kryptografian toteuttamista. 

## Miten tehdä

Satunnaislukujen luomiseen on olemassa monia tapoja Rubyssa, mutta yleisimmin käytetyt ovat `rand` ja `srand` -funktiot. Seuraavassa on esimerkki koodista, joka generoi kolme satunnaista kokonaislukua välillä 1-10:

```Ruby
3.times do
  puts rand(1..10)
end
```

Tämä koodi palauttaa seuraavanlaisen tulosteen:

```
6
2
9
```

Voit myös käyttää `srand` -funktiota asettamaan satunnaislukujen siemenen, jolloin sama koodi tuottaa aina saman tuloksen. Esimerkiksi:

```Ruby
srand 123
3.times do
  puts rand(1..10)
end
```

Tulostus olisi tässä tapauksessa aina:

```
8
10
3
```

## Syvällisempi sukellus

Satunnaislukujen generointi perustuu yleensä laskennallisiin algoritmeihin, jotka perustuvat aiempaan siemenarvoon tai satunnaislukujen sarjaan. Tämän vuoksi samalla siemenarvolla generoituja satunnaislukuja kutsutaan pseudo-satunnaisluvuiksi, sillä niiden syntymiseen on olemassa tietty säännönmukaisuus. 

Ruby käyttää Mersenne twister -algoritmia satunnaislukujen generointiin. Tämä algoritmi on hyvin tehokas ja luotettava, mutta on tärkeää muistaa, että satunnaislukujen luominen voi olla haasteellista ja tarpeisiin soveltuvaa algoritmia on harkittava tapauskohtaisesti.

## Katso myös

- Ruby:n viralliset ohjelmointiohjeet: https://www.ruby-lang.org/en/documentation/ri/
- Mersenne twister -generaattorin selitys: https://en.wikipedia.org/wiki/Mersenne_Twister