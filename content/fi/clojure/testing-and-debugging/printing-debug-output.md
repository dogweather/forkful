---
aliases:
- /fi/clojure/printing-debug-output/
date: 2024-01-20 17:52:37.337410-07:00
description: "Printtaus debuggaustarkoituksiin on virheiden j\xE4ljityst\xE4 koodista\
  \ tulostusten avulla. Koodarit k\xE4ytt\xE4v\xE4t sit\xE4, koska se auttaa ymm\xE4\
  rt\xE4m\xE4\xE4n, miss\xE4 homma\u2026"
lastmod: 2024-02-18 23:09:07.231228
model: gpt-4-1106-preview
summary: "Printtaus debuggaustarkoituksiin on virheiden j\xE4ljityst\xE4 koodista\
  \ tulostusten avulla. Koodarit k\xE4ytt\xE4v\xE4t sit\xE4, koska se auttaa ymm\xE4\
  rt\xE4m\xE4\xE4n, miss\xE4 homma\u2026"
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Printtaus debuggaustarkoituksiin on virheiden jäljitystä koodista tulostusten avulla. Koodarit käyttävät sitä, koska se auttaa ymmärtämään, missä homma menee metsään ja miksi.

## How to: (Kuinka tehdä:)
Clojure tarjoaa `println` funktiota, kun tarvitset katsoa, mitä koodissasi tapahtuu. 

```Clojure
; Yksinkertainen esimerkki tulostuksesta
(println "Hei, täällä ollaan!")

; Muuttujan arvon tarkistus
(def my-value 10)
(println "Muuttujan arvo on:" my-value)

; Funktion sisäisen tilan tarkistus
(defn my-function [x]
  (println "Funktion argumentti on:" x)
  (+ x 5))
  
(my-function 3)  ; Output: "Funktion argumentti on: 3"
```

Tulostus näkyy REPL:ssä tai komentorivillä.

## Deep Dive (Sukellus syvemmälle)
Alkujaan, printtaus debuggaukseen oli yksi harvoista keinoista ymmärtää ohjelman toimintaa. Nykyään on olemassa kehittyneempiä työkaluja, kuten interaktiiviset debuggerit, mutta `println` ja sen kaverit ovat yhä käteviä nopeisiin tarkistuksiin ja loogisten virheiden jäljitykseen.

Päällekkäisten tulosteiden välttämiseksi voi käyttää `prn` (tulostaa lisätietoja, kuten merkkijonon lainausmerkit) tai `print` (ei lisää rivinvaihtoa loppuun). Loggausekirjastot tarjoavat myös hienovaraisemmat kontrollit tulostukseen, esimerkiksi voidaan valita tulostus vakavuuden perusteella (info, warning, error).

Clojuren funktioiden puhtaasti toiminnallinen luonteisuus tarkoittaa sitä, että sivuvaikutukset, kuten `println`, on suunniteltu käytettäväksi varoen, jottei funktioiden ennustettavuus ja toistettavuus kärsi.

## See Also (Katso myös)
- Clojuren dokumentaatio println-funktioista: https://clojuredocs.org/clojure.core/println
- Interaktiivinen Clojure-debuggeri: https://cider.mx/
- Timbre, suosittu Clojure loggauskirjasto: https://github.com/ptaoussanis/timbre
- Virallinen opas Clojuren debuggaukseen: https://clojure.org/guides/debugging
