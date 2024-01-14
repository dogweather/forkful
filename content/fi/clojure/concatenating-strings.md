---
title:    "Clojure: Stringien yhdistäminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Yhdistettyjä merkkijonoja käytetään usein ohjelmoinnissa tekstipohjaista tietoa, kuten käyttäjän antamia syötteitä, tallentamiseen ja näyttämiseen. Merkkijonojen yhdistäminen mahdollistaa monipuolisemman ja käyttäjäystävällisemmän ohjelman luomisen.

## Miten tehdä

Yhdistettyjen merkkijonojen luominen Clojure-kielellä on helppoa käyttäen `str` -funktiota. Se ottaa vastaan halutut merkkijonot parametreina ja yhdistää ne yhdeksi kokonaisuudeksi.

```Clojure
(str "Tervetuloa" " blogiin!")
;; Output: "Tervetuloa blogiin!"
```

Voit myös käyttää `format` -funktiota, joka mahdollistaa merkkijonon muotoilun, kuten numeroiden lisäämisen tai päivämäärän muuttamisen halutulle muodolle.

```Clojure
(format "Tänään on %1$td/%1$tm/%1$ty" (java.util.Date.))
;; Output: "Tänään on 26/04/21"
```

## Syvempi sukellus

Clojure-kielellä merkkijonojen yhdistäminen on tehokasta ja helppoa suorittaa. Clojuren sisäänrakennettuna toiminto se myös suoriutuu nopeasti verrattuna muihin ohjelmointikieliin. Lisäksi `str` -funktion lisäksi Clojuresta löytyy useita muita toimintoja merkkijonojen manipulointiin, kuten `substring` ja `replace`.

## Katso myös

- [Clojure.org](https://clojure.org/)
- [Clojurescript.org](https://clojurescript.org/)
- [Clojure - The Essential Reference](https://www.amazon.com/Clojure-Essential-Reference-John-Russell/dp/1430266127)