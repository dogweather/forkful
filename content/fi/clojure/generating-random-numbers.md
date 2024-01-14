---
title:                "Clojure: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi Clojure-ohjelmoijat hyödyntävät satunnaislukujen generointia. Se voi auttaa luomaan satunnaisia testitietoja, simuloimaan sattumanvaraista käyttäytymistä tai jopa lisäämään viihdyttävyyttä pelinkehityksessä.

## Kuinka

Satunnaislukujen generointi Clojurella on helppoa. Voit käyttää Clojuren sisäänrakennettuja funktioita, kuten (rand) tai (rand-int), jotka generoivat desimaali- tai kokonaislukuja välillä 0-1 tai haluamasi välillä. Voit myös määrittää siemenarvon, jotta saat saman satunnaislukujen järjestyksen jokaisella suorituskerralla.

```Clojure
(rand) ; 0.599144716243095

(rand-int 10) ; 5

(rand 2 10) ; 8.067517024084043

(rand 2 10 :seed 123) ; 7.98540171249794
```

Voit myös käyttää (rand-nth) funktiota valitsemaan satunnaisen alkion vektorista tai listasta.

```Clojure
(rand-nth ["Hei" "Hello" "Bonjour"]) ; "Hei"

(rand-nth [1 2 3 4 5]) ; 3
```

## Syvempi sukellus

Clojuren satunnaislukufunktioita perustuvat Java-kieleen. Käytännössä ne hyödyntävät Mersenne Twister -generaattoria luodakseen hyvälaatuisia satunnaislukuja. Voit myös käyttää Java Random-luokkaa, jos tarvitset tarkempaa kontrollia.

Satunnaislukufunktioilla on kuitenkin myös joitakin rajoituksia. Niitä ei esimerkiksi tule käyttää salaussovelluksissa, sillä ne eivät ole täysin satunnaisia ja siksi eivät ole turvallisia salauksessa.

## Katso myös

- [Clojuren virallinen dokumentaatio satunnaislukufunktioista](https://clojuredocs.org/clojure.core/rand)
- [Mersenne Twister -generaattorin selitys](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Java Random-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)