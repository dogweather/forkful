---
title:    "Clojure: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Joskus on tarpeen muuttaa päivämäärä merkkijonoksi Clojure-ohjelmassa. Tämä voi olla tarpeen esimerkiksi tulostettaessa päivämäärää käyttäjän luettavaan muotoon tai tallentaessa sitä tietokantaan.

## Näin teet sen

```Clojure
;; Otetaan käyttöön päivämäärän muodostus kirjasto
(require '[clojure.java-time :as jtime]) 

;; Luo uusi päivämäärä 
(def date (jtime/current-inst)) 

;; Muunna päivämäärä merkkijonoksi
(def date-str (jtime/format "dd/MM/yyyy" date))

;; Tulosta tulos 
(println date-str) ;; Output: 25/11/2021
```

## Syvä sukellus

Muuntaessa päivämäärää merkkijonoksi, on tärkeä ymmärtää käytössä oleva päivämääräformaatti. Clojure-ohjelmassa päivämäärän muuttaminen merkkijonoksi tapahtuu käyttäen `format` funktiota ja antamalla haluttu formaatti merkkijonona. Valinnaisena parametrina voi myös antaa aikavyöhykkeen ja sijainnin, jotka vaikuttavat päivämäärän esitystapaan.

## Katso myös

- [Clojure-ohjelmoinnin aloitusopas](https://clojure.org/guides/getting_started)
- [Päivämäärän käsittelyn opas Clojurella](https://clojuredocs.org/clojure.instant)