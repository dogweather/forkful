---
title:                "Clojure: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi muuntaa merkkijono pienikirjaimiseksi? Pienikirjaimistaminen voi olla hyödyllistä esimerkiksi tietokannan haut tai vertailutarkoituksissa.

## Miten tehdä

Klojurella merkkijonon pienikirjaimistaminen on helppoa käyttämällä `clojure.string/lower-case`-funktiota. Katso esimerkkejä ja tulostuksia alla olevasta koodilohkosta.

```Clojure
(clojure.string/lower-case "HELSINKI")
;; Tulostaa "helsinki"

(clojure.string/lower-case "CLOJURE PROGRAMMOINTI")
;; Tulostaa "clojure ohjelmointi"
```

Voit myös käyttää `str/lower-case`-funktiota, joka toimii samalla tavalla.

```Clojure
(str/lower-case "ESIMERKKI")
;; Tulostaa "esimerkki"
```

## Syvemmälle

Merkkijonon pienikirjaimistaminen ei rajoitu pelkästään aakkosiin. Esimerkiksi ääkköset ja erikoismerkit voidaan myös muuntaa pienikirjaimiksi.

```Clojure
(clojure.string/lower-case "ÄÄKKÖSET")
;; Tulostaa "ääkköset"

(clojure.string/lower-case "PELIAIKA: 00:20.5")
;; Tulostaa "peliaika: 00:20.5"
```

On myös hyvä huomata, että merkkijono pysyy muuttumattomana, ellei sitä tallenneta uuteen muuttujaan tai rakenteeseen.

```Clojure
;; Alkuperäinen merkkijono
(def city "TAMPERE")

;; Pienikirjaimistettu merkkijono tallennetaan uuteen muuttujaan
(def lowercase-city (clojure.string/lower-case city))

(lowercase-city)
;; Tulostaa "tampere"

city
;; Pysyy muuttumattomana ja tulostaa silti "TAMPERE"
```

## Katso myös

- [Klojure - String-oppaat](https://clojure.org/guides/strings)
- [Klojure - Merkkijonofunktiot](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/lower-case)