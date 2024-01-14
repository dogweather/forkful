---
title:                "Clojure: Alipalamerkkien erottaminen."
simple_title:         "Alipalamerkkien erottaminen."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi käyttää substringien louhintaan Clojurea? Substringien louhinta on tärkeä osa tietojenkäsittelyä, jossa tietyn osan tiedosta halutaan erottaa ja käsitellä erikseen. Clojurella voit tehdä tämän helposti ja tehokkaasti.

## Kuinka

Clojuren `subtring`-funktio tarjoaa helpon tavan louhia tai erottaa osa tekstistä. Alla on yksinkertainen esimerkki, jossa haemme substringin merkkien "Clojure" välillä olevasta lauseesta "Tämä on lause Clojurella".

```Clojure
(println (subs "Tämä on lause Clojurella" 10 17))
;; tuottaa tuloksen "Clojure"
```

Voit myös käyttää `substring`-funktiota samalla tavalla:

```Clojure
(println (substring "Tämä on lause Clojurella" 9 16))
;; tuottaa tuloksen "Clojure"
```

Voit myös käyttää negatiivisia indeksejä määrittääksesi aloitus- ja lopetusindeksit loppupäästä.

```Clojure
(println (substring "Tämä on lause Clojurella" 9 -2))
;; tuottaa tuloksen "Clojure"
```

## Syvemmälle

Substringin louhiminen ei rajoitu pelkästään yhden sanan palauttamiseen. Voit myös käyttää regex-malleja löytääksesi tiettyjä osia tekstistä. Esimerkiksi, voit käyttää `re-find`-funktiota saadaksesi ensimmäisen löydetyn substringin, joka vastaa regex-mallia.

```Clojure
(def lause "Ohjelmointikieli Clojure on suosittu funktionaalinen kieli")

(println (re-find #"[Cc]lojure" lause))
;; tuottaa tuloksen "Clojure"
```

Samoin voit käyttää `re-seq`-funktiota saadaksesi kaikki löydetyt substringit regex-mallin perusteella.

```Clojure
(def lause "Ohjelmointikieli Clojure on suosittu funktionaalinen kieli")

(println (re-seq #"[a-z]+" lause))
;; tuottaa tuloksen ("Clojure" "on" "suosittu" "funktionaalinen" "kieli")
```

## Katso myös

- [Clojure dokumentaatio](https://clojure.org/)
- [Regex-mallit Clojurella](https://clojuredocs.org/clojure.core/re-seq)