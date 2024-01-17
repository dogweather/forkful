---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Clojure: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Säännölliset lausekkeet ovat työkalu, jota ohjelmoijat voivat käyttää merkkijonojen käsittelyyn. Ne antavat mahdollisuuden tarkistaa, löytää ja muokata tiettyjä merkkijonoja tekstidatasta. Tämä tekee niistä erittäin hyödyllisen työkalun, kun halutaan suorittaa tiettyjä tehtäviä automaattisesti ja tehokkaasti.

# Miten:

```Clojure
;; Määritellään säännöllinen lauseke
(def regex #"[a-z]+")

;; Tarkistetaan, täsmäävätkö merkkijonot lausekkeeseen ja palautetaan
;; löydetty osa
(re-matches regex "Hei maailma")
;; => "Hei"

;; Korvataan löydetyt osat toisella merkkijonolla
(defn replace-text [text]
  (let [regex #"[aeiouyäöå]" ;; Määritellään vokaalit
        replace-fn (fn [m] (str *m \*))] ;; Korvausfunktio
    (clojure.string/replace text regex replace-fn)))

(replace-text "Hei maailma")
;; => "H** m***lm*"
```

# Syväsukellus:

Säännöllisiä lausekkeita käytetään laajasti niin ohjelmoinnissa kuin tekstitiedostojen käsittelyssä. Niiden käyttöön liittyy kuitenkin joitakin haasteita, kuten vaikeaselkoisuus ja virhetilanteiden hallinta. Tärkeää onkin ymmärtää säännöllisten lausekkeiden rakenteita ja syntaksia, jotta niitä osaa käyttää oikein ja tehokkaasti.

On myös olemassa vaihtoehtoisia tapoja käsitellä merkkijonoja, kuten funktioita ja kirjastoja, joita voi käyttää yhdessä Clojuren tarjoamien ominaisuuksien kanssa. Näiden avulla voi löytää itselleen sopivan menetelmän merkkijonojen käsittelyyn, joten kannattaa tutustua eri vaihtoehtoihin.

Säännöllisten lausekkeiden toteutus Clojuressa perustuu Java-kielen RegExp-kirjastoon, joten siitä löytyy runsaasti dokumentaatiota ja esimerkkejä. Lisäksi Clojure tarjoaa omia funktioita lausekkeiden manipulointiin ja käsittelyyn, mikä helpottaa niiden käyttöä.

# Katso myös:

- [Clojure-documentation](https://clojure.org/index)
- [Java RegExp-kirjasto](https://docs.oracle.com/javase/9/docs/api/java/util/regex/package-summary.html)
- [Clojure string-functions](https://clojuredocs.org/clojure.string)