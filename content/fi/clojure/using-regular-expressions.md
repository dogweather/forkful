---
title:                "Clojure: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Regular Expression eli säännöllinen lauseke on voimakas työkalu tekstinkäsittelyssä. Se mahdollistaa tarkkojen haku- ja korvaustoimintojen tekemisen tekstissä. Säännölliset lausekkeet ovat suosittuja ohjelmointikielistä riippumatta ja niiden oppiminen on hyödyllistä monessa eri ohjelmointiympäristössä.

## Miten

Regular Expressionit ovat osa Clojuren ydinbibliotekkia ja niitä voi käyttää "trygemin" funktiolla. Käytännössä säännöllinen lauseke annetaan merkkijonona ja sen kanssa käytetään sitten monia erilaisia funktioita, jotka suorittavat erilaisia hakutoimintoja. Esimerkiksi voit etsiä tietyn kuvion tekstistä tai korvata sen toisella.

```Clojure
;; etsii kaikki numerot sanasta "12345"
(re-find #"\d+" "12345") ; "12345"

;; korvaa merkkijonon "world" merkkijonolla "universe"
(re-place #"world" "hello world" "world" "universe") ; "hello universe"
```

## Syvempää sukellusta

Säännöllisten lausekkeiden merkintätavoissa on eroja eri ohjelmointikielien välillä, mutta Clojuren tapauksessa käytössä on Perl-yhteensopiva merkintätapa. Tämä tarkoittaa, että perinteiset säännöllisen lausekkeen merkit toimivat myös Clojuressa. Esimerkiksi `+` merkki tarkoittaa "yhdestä useaan kertaa esiintyvää" ja `*` merkki taas "ei-yhtään tai useampaa kertaa esiintyvää".

Säännöllisten lausekkeiden käyttö voi aluksi tuntua haastavalta, mutta niiden opettelu palkitsee pitkällä tähtäimellä. Niitä voi käyttää monipuolisesti kaikentyyppisessä tekstikäsittelyssä ja ne tehostavat ohjelmointikokemusta merkittävästi.

## Katso myös

- [Clojuren virallinen dokumentaatio regular expressioneistä](https://clojuredocs.org/clojure.core/re-pattern)
- [Vinkkejä regular expressionien käyttöön Clojuressa](https://stackoverflow.com/questions/40197139/using-re-seq-with-match-groups-in-clojure/40197724#40197724)
- [Tutoriaali regular expressioneista ja niiden soveltamisesta käytännössä](https://coderwall.com/p/fdvcjq/effective-regular-expressions-in-clojure)