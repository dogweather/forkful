---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Clojure: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Mikä & Miksi? 
Haku ja korvaaminen on tärkeä osa ohjelmointia, jossa tekstiä etsitään ja korvataan tietyillä säännöillä tai hakuavaimilla. Tämä voi säästää paljon aikaa ja vaivaa, kun kyseessä on suurten tekstimäärien käsittely.

Kuinka: 
Tässä on muutama esimerkki siitä, kuinka voit suorittaa hakuja ja korvauksia Clojure-kielessä:

```Clojure
;; Korvaa kaikki esiintymät merkkijonolla "hello" merkkijonossa "world"
(clojure.string/replace "hello world world" #"world" "goodbye")

Tulostaa: "hello goodbye goodbye"

;; Etsi kaikki luvut, jotka ovat pienempiä kuin 10 ja korvaa ne merkkijonolla "pieni"
(clojure.string/replace "1 5 10 15" #"[0-9]+" "pieni" :count 2)

Tulostaa: "pieni pieni 10 15"

;; Käytä säädyllistä korvausfunktiota korvaamiseen
(clojure.string/replace "hello world" #"[a-z]+" (fn [_] "goodbye"))

Tulostaa: "goodbye goodbye"
```

Deep Dive: 
Haku ja korvaaminen ovat tärkeitä ohjelmoinnin toimintoja, ja niitä on käytetty jo vuosikymmenien ajan. Yhä useammat kielet, kuten Clojure, tarjoavat valmiita funktioita näiden toimintojen suorittamiseen, jolloin ohjelmoijien ei tarvitse kirjoittaa omia haku- ja korvaustoimintojaan.

On myös muita tapoja suorittaa hakuja ja korvauksia, kuten regular expression -kielellä, joka tarjoaa enemmän joustavuutta sääntöjen määrittelyssä. Clojure tarjoaa myös muita funktioita, kuten ```clojure.string/replace-first```, joka korvaa vain ensimmäisen esiintymän, ja ```clojure.string/replace-nth```, jolla voidaan korvata tietty määrä esiintymiä.

See Also:
Lisätietoja hakujen ja korvaamisen suorittamisesta Clojure-kielessä löytyy virallisesta dokumentaatiosta: https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/replace. Voit myös löytää lisää tietoa regular expressionista ja sen käytöstä hakujen ja korvausten tekemiseen esimerkiksi täältä: https://www.regular-expressions.info/clojure.html.