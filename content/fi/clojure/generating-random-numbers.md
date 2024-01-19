---
title:                "Satunnaisten numeroiden luominen"
html_title:           "Bash: Satunnaisten numeroiden luominen"
simple_title:         "Satunnaisten numeroiden luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Satunnaislukujen luominen on prosessi, jossa tuotetaan luku satunnaisella periaatteella, eikä ennalta määrättyjen sääntöjen mukaan. Ohjelmoijat tarvitsevat tätä toimintoa usein monissa erilaisissa tilanteissa, kuten pelien luomisessa, tilastollisissa analyyseissa tai turvallisuussyistä.

## Näin teet:

Clojure tarjoaa useita tapoja luoda satunnaislukuja. Yksinkertaisin on käyttää `rand` -funktiota, kuten nähdään alla:

```clojure
(rand) 
```
Tämä palauttaa satunnaisen liukuluvun väliltä 0.0 ja 1.0. Näin ollen, saatat saada jotakin tällaista:

```clojure
0.3940655398820112
```

Jos haluat satunnaisen kokonaisluvun, voit yhdistää tämän `int` -funktioon:

```clojure
(int (rand 10)) 
```

Esimerkiksi tämä palauttaa satunnaisen kokonaisluvun väliltä 0 ja 9.

## Syvenny:

Historiallisessa yhteydessä, satunnaislukugeneraattorit ovat olleet tombstone-ohjelmoinnin pilarit, ja niitä on käytetty useissa erilaisissa sovelluksissa, peleistä simulointeihin. Nämä generaattorit perustuvat yleensä johonkin matemaattiseen algoritmiin, ja niiden laatu riippuu algoritmin valinnasta.

Clojuren `rand`-funktio hyödyntää alla olevan Javan Random-luokkaa, joka on pseudo-satunnaislukugeneraattori. Niitä kutsutaan pseudoiksi, koska ne tuottavat sarjan lukuja, joka näyttää satunnaiselta mutta toistuu lopulta. 

Clojuressa on lisäksi olemassa many other libraries, kuten [test.check](https://github.com/clojure/test.check) ja [java.util.concurrent.ThreadLocalRandom](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html), jotka tarjoavat erilaisia työkaluja ja tekniikoita satunnaislukujen luomiseen.

## Lisätietoja:

- [Clojure Docs: Random](https://clojuredocs.org/clojure.core/rand)
- [Test Check Repo](https://github.com/clojure/test.check)
- [Oracle Java 8 ThreadLocalRandom](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)