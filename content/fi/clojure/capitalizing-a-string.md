---
title:    "Clojure: Merkkijonon muuttaminen isoine kirjaimin"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi capitalizing-merkkijonon kirjoittaminen on hyödyllistä?

Capitalizing-merkkijonon kirjoittaminen tarkoittaa, että muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi ja kaikki muut kirjaimet pieniksi kirjaimiksi. Tämä voi olla hyödyllistä esimerkiksi tekstikäsittelyohjelmassa, jossa halutaan korostaa tiettyä sanaa tai lauseketta.

## Kuinka tehdä se:

Capitalizing-merkkijonon voi tehdä helposti Clojurella käyttämällä "upper-case" funktiota ja "join" funktiota. Esimerkiksi:

```Clojure

(def sana "tämä on capitalizing-merkkijono")
(-> sana
   (clojure.string/split #" ")
   (map (partial apply str))
   (map #(clojure.string/upper-case (first %) (apply str (rest %))))
   (join " "))

; => "Tämä On Capitalizing-Merkkijono"
```

## Syvällinen tarkastelu:

Capitalizing-merkkijonon kirjoittaminen on yksi monista stringin manipuloimisen tavoista Clojurella. Monet muut funktiot, kuten "lower-case" ja "capitalize", ovat myös hyödyllisiä erilaisissa tilanteissa. Lisäksi, Clojurella on monia muita hyödyllisiä funktioita, kuten "replace" ja "substring", joita voi käyttää eri tavoin muokataksesi merkkijonoja.

### Katso myös:

- [Clojure string functions](https://clojuredocs.org/clojure.core#string-functions)
- [Complete guide to strings in Clojure](https://cljdoc.org/d/clojure/clojure/1.10.1-alpha1/doc/guides/strings)
- [Clojure cheetsheet](https://clojure.org/api/cheatsheet) (haku "strings")