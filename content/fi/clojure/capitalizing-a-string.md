---
title:                "Clojure: Tekstin ensimmäisen kirjaimen muuttaminen isoksi"
simple_title:         "Tekstin ensimmäisen kirjaimen muuttaminen isoksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi 

Oletko koskaan miettinyt, miksi joskus haluat muuttaa merkkijonon alkukirjaimen isoksi? Tämä voi olla hyödyllistä esimerkiksi käsitellessä käyttäjän syöttämiä tietoja tai luodessa otsikoita. Clojure tarjoaa helpon tavan tehdä tämä, joten lue eteenpäin oppiaksesi kuinka!

## Kuinka

```Clojure
(defn capitalize [s]
  (str (clojure.string/capitalize s)))

(capitalize "hei, tämä on esimerkki")
;; Output: "Hei, tämä on esimerkki"
```

Koodinpätkä näyttää funktion, nimeltään "capitalize", joka ottaa parametrin s ja käyttää Clojuren string-kirjastoa muuttaakseen merkkijonon ensimmäisen kirjaimen isoksi. Voit sitten kutsua tätä funktiota haluamallasi merkkijonolla ja se palauttaa alkuperäisen merkkijonon, mutta muutettuna isoksi ensimmäisen kirjaimen osalta. 

## Syvällinen sukellus

Funktion sisällä Clojure käyttää string-kirjastoa, joka tarjoaa monia hyödyllisiä funktioita merkkijonojen käsittelyyn. Clojure suosii toiminnallista ohjelmointityyliä, joten tällainen funktioiden ketjuttaminen on hyvin tavallista. Voit myös muuttaa funktion niin, että se muokkaa jokaisen sanan ensimmäistä kirjainta isoksi, käyttäen esimerkiksi "clojure.string/capitalize-words" funktiota. Vaihtoehtoisesti voit myös käyttää "words" ja "join" funktioita luodaksesi uuden merkkijonon, jossa jokainen sana alkaa isolla kirjaimella.

## Katso myös

- [Clojure string-kirjasto](https://clojure.github.io/clojure/clojure.string-api.html)
- [Clojure funktiot](https://clojuredocs.org/)