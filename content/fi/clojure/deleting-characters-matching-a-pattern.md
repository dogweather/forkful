---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkien poistaminen sovitetusta kaavasta viittaa merkkijonoista erityisten merkkien poistamiseen ohjelmoinnissa. Ohjelmoijat tekevät tämän puhdistamaan ja muodoittamaan tietoja.

## Miten tehdään:

Clojuren `clojure.string/replace`-funktio on kodinomainen keino poistaa merkkijonoista merkit. Se hyväksyy merkkijonon, kaavan ja korvikkeen, ja palauttaa uuden merkkijonon.

```Clojure
(require '[clojure.string :as s])

(defn remove-chars [str pattern]
  (s/replace str pattern ""))

(println (remove-chars "H3llo, W0rld!" #"\d")) 
```

Ohjelman tulostus on: 

```Clojure 
"Hllo, Wrld!"
```

Tässä esimerkissä digitaaliset merkit (0 ja 3) on poistettu alkuperäisestä merkkijonosta.

## Syvempi sukellus: 

Vaikka `clojure.string/replace` on käytännöllinen ja tyypillisesti riittävän nopea useimmille sovelluksille, on olemassa muitakin tapoja poistaa merkkijonosta merkit. Voit esimerkiksi rakentaa uuden merkkijonon käymällä vanhan merkkijonon läpi yksi merkki kerrallaan ja ottaa mukaan vain sopivat merkit.

Clojuren olemassaolon historiassa on ollut monia tapoja tehdä tämä. Yksi ensimmäisistä oli `clojure.string/replace-first`, joka kuitenkin käytti enemmän käyttömuistia kuin uudempi `clojure.string/replace`.

## Katso myös: 

- [Clojure Official Documentation: `clojure.string`](https://clojuredocs.org/clojure.string)
- [Clojure: `clojure.string/replace` vs `clojure.string/replace-first`](https://stackoverflow.com/questions/67114126/clojure-clojure-string-replace-vs-clojure-string-replace-first)