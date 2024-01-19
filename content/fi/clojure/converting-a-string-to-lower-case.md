---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon muuttaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkkijonon kirjaimet vaihdetaan pieniksi kirjaimiksi. Tätä käytetään usein, kun halutaan käsitellä merkkijonoja tapausriippumattomasti, kuten haku- ja vertailutoiminnoissa.

## Näin teet:

Clojure-langossa String-objektien muuttaminen pieniksi kirjaimiksi tapahtuu `clojure.string/lower-case` -funktion avulla. Tässä on esimerkkikoodi:

```Clojure
(require '[clojure.string :as str])
(defn to-lower-case [s]
  (str/lower-case s))

;; Testaa funktiota 
(print (to-lower-case "Hei Maailma"))
;; Tulostaa: "hei maailma"
```

## Syvällisemmin:

(1) Historiallinen konteksti: Merkkijonon pieniksi kirjaimiksi muuttaminen on perusominaisuus, joka on ollut käytettävissä monissa ohjelmointikielissä.

(2) Vaihtoehdot: Voimme käyttää Clojuren `map` ja `Character/toLowerCase` toimintojen yhdistelmää, jos haluamme:

```Clojure
(defn to-lower-case-v2 [s]
  (apply str (map Character/toLowerCase s)))
```

(3) Toteutusdetailit: `clojure.string/lower-case`-funktio kutsuu Javan String alustan `toLowerCase`-funktiota, joka muuttaa kaikki isot kirjaimet pieniksi kirjaimiksi.

## Katso myös:

- API-dokumentaatio: https://clojuredocs.org/clojure.string/lower-case
- StackOverflow-keskusteluista: https://stackoverflow.com/questions/3395294/transforming-string-casing-in-clojure