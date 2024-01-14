---
title:                "Clojure: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Usein tämä tapahtuu, kun tarvitsemme muuttaa tai puhdistaa dataa, joka sisältää ylimääräisiä tai tarpeettomia merkkejä.

## Kuinka se tehdään

Poistaaksesi merkit vastaavat kaavaa, voit käyttää `replace`-funktiota yhdessä `re-seq`-funktion kanssa. `replace` korvaa kaikki vastaavat merkit toisella merkillä tai tyhjällä merkkijonolla. `re-seq` kerää kaikki kaavan mukaiset merkit listaksi.

```Clojure
;; Määritellään muuttuja, joka sisältää tiedot, joista haluamme poistaa välilyönnit

(def data "He ll o wo rl d")

;; Luo listan välilyönneistä

(def whitespaces " ")

;; Korvaa välilyönnit tyhjillä merkkijonoilla

(replace data whitespaces "")

;; Tulostaa "HelloWorld"
```

Voit myös käyttää säännöllisiä lausekkeita poistaaksesi haluamasi kaavan mukaiset merkit. Esimerkiksi, jos haluat poistaa kaikki numerot datasta, voit käyttää `[0-9]`-kaavaa.

```Clojure
(def data "Text with 1 number")

(replace data #"[0-9]" "")

;; Tulostaa "Text with number"
```

## Syvällisempi sukellus

Poistaessa merkkejä vastaavalla kaavalla, on tärkeää ymmärtää säännöllisiä lausekkeita. Voit käyttää erilaisia kaavoja sen perusteella, mitä haluat poistaa, kuten eri merkkejä tai merkkijonoja. Lisäksi `replace`- ja `re-seq`-funktiot ovat hyödyllisiä yhdistettynä säännöllisten lausekkeiden kanssa.

## Katso myös

- [ClojureDocs: replace](https://clojuredocs.org/clojure.core/replace)
- [ClojureDocs: re-seq](https://clojuredocs.org/clojure.core/re-seq)
- [Regular Expression Tutorial](https://regexone.com/) (säännöllisten lausekkeiden opas)