---
title:                "Merkkijonojen yhdistäminen"
date:                  2024-01-20T17:34:34.948285-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Yhdistäminen ("string concatenation") on useiden merkkijonojen liittämistä yhteen. Koodarit tarvitsevat tätä toimintoa muodostaakseen yksittäisistä palasista tarpeellisia viestejä tai dataa.

## How to:
Clojuren merkkijonojen yhdistäminen voi tapahtua useilla tavoilla. Tässä muutamia esimerkkejä:

```Clojure
;; Käyttäen str-funktiota yksinkertaiseen yhdistämiseen:
(str "Hei, " "maailma!")
;; => "Hei, maailma!"

;; Yhdistäminen liittämällä muuttujia ja vakioita:
(def tervehdys "Hei")
(def kohde "maailma")
(str tervehdys ", " kohde "!")
;; => "Hei, maailma!"

;; Useiden merkkijonojen yhdistäminen map- ja join-funktioilla:
(clojure.string/join " " ["Tervetuloa" "Clojure" "maailmaan!"])
;; => "Tervetuloa Clojure maailmaan!"
```

## Deep Dive
Merkkijonojen yhdistäminen on ollut ohjelmoinnin perustyökalu alkuajoista lähtien. Clojuressa `str` on standardimenetelmä, joka ottaa mielivaltaisen määrän argumentteja ja yhdistää ne merkkijonoksi.

Vaihtoehtoiset tavat, kuten `clojure.string/join`, on hyvä, kun sinulla on kokoelma merkkijonoja, jotka haluat erottaa jonkin merkkijonon avulla. Se hyödyntää Javan `StringBuilder` luokkaa tehokkuuden takia.

Toteutusyksityiskohtina, `str`-funktion tehokkuus riippuu siitä, kuinka Clojure ja lopulta Java yhdistävät merkkijonot. Nykyisin tämä on optimoitu Java Virtual Machinessa StringBuilderin käytön myötä, mikä mahdollistaa joustavuuden ilman, että suorituskyky kärsii liikaa.

## See Also
- Clojuren virallinen dokumentaatio: [https://clojure.org/](https://clojure.org/)
- `clojure.string/join` funktion dokumentaatio: [https://clojuredocs.org/clojure.string/join](https://clojuredocs.org/clojure.string/join)
- Java `StringBuilder` luokka: [https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)