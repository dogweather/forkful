---
date: 2024-01-20 17:57:56.274103-07:00
description: "How to: Clojurissa k\xE4ytet\xE4\xE4n `clojure.string/replace` funktiota\
  \ tekstinkorjaukseen. T\xE4ss\xE4 pari esimerkki\xE4."
lastmod: '2024-03-13T22:44:56.170487-06:00'
model: gpt-4-1106-preview
summary: "Clojurissa k\xE4ytet\xE4\xE4n `clojure.string/replace` funktiota tekstinkorjaukseen."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to:
Clojurissa käytetään `clojure.string/replace` funktiota tekstinkorjaukseen. Tässä pari esimerkkiä:

```clojure
(require '[clojure.string :as str])

; Yksinkertainen korvaus
(str/replace "Moikka maailma" "maailma" "Clojure")
; Output: "Moikka Clojure"

; Säännöllisiin lausekkeisiin perustuva korvaus
(str/replace "Etsi numeroita 123 tästä" #"\d+" "NUMERO")
; Output: "Etsi numeroita NUMERO tästä"
```

## Deep Dive
Tekstin etsiminen ja korvaaminen on vanha käytäntö, ulottuen editorien kuten ed ja sed varhaisiin päiviin Unix-järjestelmissä. Clojure, moderni Lispin sukupolvi, käsittelee tekstin korvauksia funktionaalisesti.

Vaihtoehtoina Clojurelle, voi käyttää toisaalta tekstieditoreita tai komentorivin työkaluja kuten `sed` ja `awk`. Clojure eroaa niistä immutaabilisuutensa ja laiskan evaluoinnin avulla, mikä mahdollistaa tehokkaan datankäsittelyn.

Yksityiskohdista, `clojure.string/replace` voi ottaa myötä funktiot, jotka määrittävät korvauslogiikan dynaamisemmin. Esimerkiksi:

```clojure
(str/replace "Joku teksti jossa on numeroita 123 ja 456"
             #"\d+"
             (fn [match] (str "<<" match ">>")))
; Output: "Joku teksti jossa on numeroita <<123>> ja <<456>>"
```

## See Also
- [ClojureDocs `clojure.string/replace`](https://clojuredocs.org/clojure.string/replace)
- [Regular expressions in Clojure](https://www.braveclojure.com/functional-programming/#Regular_Expressions)
