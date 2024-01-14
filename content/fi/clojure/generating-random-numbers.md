---
title:    "Clojure: Satunnaislukujen generointi"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Jos olet ohjelmoija tai haluat oppia ohjelmointia, saatat törmätä tilanteisiin, joissa tarvitset satunnaisesti generoituja numeroita. Esimerkiksi simulaatioissa tai tietokilpailupeleissä tarvitset lukuja, jotka eivät ole ennalta määrättyjä. Tämän vuoksi taito generoida satunnaisia lukuja on hyödyllinen taito jokaiselle ohjelmoijalle.

## Miten

Satunnaisluvut voidaan generoida Clojurella käyttämällä funktiota ```random```, joka hyväksyy parametrina muuttujan ja palauttaa siihen liittyvän satunnaisen luvun.

```Clojure
(def number (random 10)) 
;; Palauttaa satunnaisluvun väliltä 0 - 9
```

Voit myös generoida tietyn määrän satunnaisia lukuja käyttämällä ```repeat```-funktiota yhdessä ```random```-funktion kanssa:

```Clojure
(repeat 5 (random 100))
;; Palauttaa viisi satunnaista lukua väliltä 0 - 99
```

Voit myös antaa ```random```-funktiolle parametreina haluamasi ala- ja ylärajan satunnaislukujen generoimiseksi:

```Clojure
(random 50 100)
;; Palauttaa satunnaisluvun väliltä 50 - 99
```

## Sukella syvemmälle

Clojuressa on myös mahdollista generoida erilaisia satunnaislukuja erilaisilla jakaumilla käyttämällä "```probabilistic```"-kirjastoa. Voit asentaa tämän kirjaston käyttämällä Clojurensa sisäänrakennettua työkalua "```deps.edn```" tai Leiningenin tai Bootin avulla. Tämän jälkeen voit käyttää esimerkiksi "```probabilistic.gaussian```"-funktiota palauttamaan satunnaisluvun normaalijakaumasta:

```Clojure
(use 'probabilistic.gaussian)
(normal 50 10)
;; Palauttaa satunnaisluvun, jonka keskiarvo on 50 ja hajonta on 10
```

## Katso myös

- [Clojuren viralliset dokumentit random-funktiosta](https://clojuredocs.org/clojure.core/random)
- [Probabilistic-kirjaston dokumentaatio](https://github.com/clojure-numerics/probabilistic)