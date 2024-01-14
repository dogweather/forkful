---
title:    "Clojure: Tulostaminen virheenkorjauksellista lähtöä"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Tämä artikkeli käsittelee debug-tulostuksen käyttöä Clojure-ohjelmoinnissa. Miksi sitten tarvitsisimme tulostaa debug-tietoja? Yksinkertaisesti sanottuna, debug-tulostus auttaa meitä selvittämään ohjelmassa esiintyviä virheitä ja löytämään mahdollisia bugeja. Tämä on erittäin hyödyllistä ohjelmia kehitettäessä ja vianetsinnässä.

## Miten

```Clojure
(defn add [x y]
  (let [result (+ x y)]
    (println "x: " x " y: " y " result: " result)
    result))

(add 5 3)
```
Tulostaa:
```
x: 5 y: 3 result: 8
```

Kun haluamme tulostaa debug-tietoja, voimme käyttää `println`-funktiota. Tämä tulostaa haluamamme muuttujat ja niiden arvot konsoliin. Huomaa, että tämä toimii vain komentorivillä, emmekä saa tulostusta näkyviin esimerkiksi graafisissa ohjelmissa.

## Syvemmälle

Debug-tulostus on hyvä työkalu, mutta sitä kannattaa käyttää harkiten. Jos ohjelma on suuri ja sisältää paljon debug-tulostuksia, se voi hidastaa ohjelman suoritusta tai aiheuttaa ylimääräistä melua konsolissa. Käytä siis debug-tulostusta vain, kun se on välttämätöntä.

Voimme myös käyttää tarkempia tulostusfunktioita, kuten `println-str`, joka palauttaa tuloksen merkkijonona sen sijaan, että tulostaisi sen konsoliin. Tämä on hyödyllistä esimerkiksi, kun haluamme tallentaa debug-tiedot lokitiedostoon.

## Katso myös
- [Clojure debug-tulostus dokumentaatio](https://clojuredocs.org/clojure.core/println)
- [Debug-tulostusohjeet Clojure-yhteisön sivustolta](https://clojure.org/guides/getting_started)