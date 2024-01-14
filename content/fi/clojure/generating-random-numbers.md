---
title:    "Clojure: Sattumanvaraisten numeroiden generointi"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Randomin numeroiden generoiminen on tärkeää ohjelmoinnissa, sillä se mahdollistaa pseudosatunnaisen tiedon luomisen. Tämä voi olla hyödyllistä esimerkiksi peleissä, arvontojen toteuttamisessa tai testitietojen generoinnissa.

## Miten

```Clojure
(import '[java.util Random])

(def r (Random.))

; Yksittäisen satunnaisen numeron generointi
(.nextInt r)

; Satunnaisen numeron generointi tiettynä väliltä
(.nextInt r 10)
```

```
9 ; esimerkki yksittäisen satunnaisen numeron kanssa
7 ; esimerkki välillä 0-10 olevan numeron kanssa
```

## Syväsukellus

Satunnaisen numeroiden generoimisen perusperiaate on samanlainen useimmissa ohjelmointikielissä. Clojuressa voidaan käyttää `Random` luokkaa, joka mahdollistaa satunnaisten lukujen generoimisen. `.nextInt` metodin avulla voidaan määrittää haluttu numeron alue, jolta satunnainen numero generoidaan.

Kiinnostavaa on se, että satunnaiset numerot eivät oikeasti ole täysin satunnaisia, vaan ne luodaan pseudosatunnaisena sekvenssinä tietyn pseudosatunnaisen numerogeneraattorin avulla. Tämä tarkoittaa sitä, että mikäli käytetään samaa pseudosatunnaisen numerogeneraattoria ja samalla alkuarvolla, saadaan aina sama numerosekvenssi.

## Katso myös

- [The Clojure Cheat Sheet](https://clojure.org/guides/learn/syntax#_language_syntactic_elements)
- [The Java Tutorials - Random Numbers](https://docs.oracle.com/javase/tutorial/java/data/manipstrings.html)