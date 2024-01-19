---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen pituuden selvittäminen tarkoittaa merkkien lukumäärän laskemista merkkijonossa. Ohjelmoijat tekevät tämän, kun tarvitaan tietoa merkkijonon rakenteesta tai kun he haluavat iteroida sen läpi.

## Näin se tehdään:

Clojure-ohjelmassa voit laskea merkkijonon pituuden käyttäen `count` funktiota.

```clojure
(defn string-length [s]
  (count s))

(println (string-length "Hei, Clojure!")) ;; Tulostaa: 13
```

## Syvempi sukellus:

Menneisyydessä, Clojuren alkuaikoina, usein käytetty tapa laskea merkkijonon pituus oli käyttää Java-interopolia ja kutsua `.length` metodia merkkijonon päällä. Tämä tapa on kuitenkin hylätty, koska `count` on natiivi Clojure-funktio ja sitä pidetään yleisesti parempana.

Vaihtoehtoisesti voit laskea merkkijonon pituuden `reduce` funktion avulla, mutta tämä on yleensä hitaampi ja monimutkaisempi kuin `count`:

```clojure
(defn string-length-2 [s]
  (reduce (fn [n _] (inc n)) 0 s))

(println (string-length-2 "Hei, Clojure!")) ;; Tulostaa: 13
```

Merkkijonojen pituuden laskeminen `count`:lla on hyvin suoraviivaista. Clojure käyttää Javan String-luokkaa merkkijonojen esittämiseen, joka tallentaa merkkijonojen pituuden sisäisesti. Kun kutsut `count` funktiota, se palauttaa tämän sisäisen arvon.

## Katso myös:

[Clojure-doc: count](https://clojuredocs.org/clojure.core/count)

[Clojure-doc: reduce](https://clojuredocs.org/clojure.core/reduce)