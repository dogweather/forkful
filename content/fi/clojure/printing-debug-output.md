---
title:                "Virheenkorjaustulosteen tulostaminen"
html_title:           "Clojure: Virheenkorjaustulosteen tulostaminen"
simple_title:         "Virheenkorjaustulosteen tulostaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteiden tulostaminen voi olla hyödyllistä ohjelmoinnin aikana, sillä se auttaa löytämään mahdollisia virheitä ja ymmärtämään ohjelman toimintaa paremmin.

## Miten

```Clojure
;; Yksinkertainen tapa tulostaa debug-tulosteita on käyttää "println" funktiota.
(println "Debug-tuloste: " (+ 1 2))

;; Voit myös käyttää "println" funktiota yhdistämällä sen merkkijonojen ja muuttujien kanssa.
(def x 5)
(println "Muuttujan x arvo on:" x)

;; Voit myös käyttää "prn" funktiota tulostamaan debug-tulosteita muodossa, joka helpompi lukea.
(prn "Tämä on debug-tuloste.")

;; Voit käyttää "printf" funktiota tulostamaan muotoillun tekstin ja muuttujien arvot.
(printf "Piin arvo on %f." Math/PI)
```

Runkokoodissa voit käyttää myös "comment" makroa tulostamaan debug-tulosteita, jotka näkyvät vain kehitysvaiheessa. Esimerkiksi:

```Clojure
(comment
  (println "Tämä debug-tuloste näytetään vain kehitysvaiheessa.")
  (prn "Tämä on toinen debug-tuloste.")
  (printf "Hei, maailma!"))
```

Tulosteen voit myös ohjata toiseen tiedostoon "spit" funktiolla. Esimerkiksi:

```Clojure
(spit "debug-tulosteet.txt" (str "Tämä on debug-tuloste numero " 1))
```

## Syvempi sukellus

Printtaus ja debug-tulosteiden tulostaminen on tärkeä osa ohjelmointia. Se auttaa sinua ymmärtämään ohjelmasi toimintaa paremmin ja löytämään mahdollisia virheitä. Voit yhdistellä erilaisia printtauksen tapoja ja valita itsellesi sopivimman tavan.

## Katso myös

- [Clojure dokumentaatio](https://clojure.org)
- [Clojure debuggaus vinkkejä](https://purelyfunctional.tv/article/how-to-debug-clojure)