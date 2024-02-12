---
title:                "Tekstin etsiminen ja korvaaminen"
aliases:
- /fi/clojure/searching-and-replacing-text.md
date:                  2024-01-20T17:57:56.274103-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstin etsiminen ja korvaaminen on prosessi, jossa löydetään tietyt merkkijonot ja korvataan ne toisilla. Ohjelmoijat käyttävät tätä tekstin päivittämiseen, virheiden korjaamiseen tai datan muotoiluun.

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
