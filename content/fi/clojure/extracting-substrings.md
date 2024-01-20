---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Alimerkkijonojen erottaminen on prosessi, jossa merkkijonosta otetaan osa sen juoksevista merkkeistä. Ohjelmoijat käyttävät tätä hyödyntääkseen joko osaa merkkijonon tiedoista tai jakamaan merkkijonon pienempiin osiin.

## Miten:

Clojurella voit käyttää `subs`:ia erottaaksesi merkkijonoja.

```Clojure
(defn näyte []
  (println (subs "Moi Clojure-ohjelmoijat!" 4 13)))
```

Kun ajat tämän funktion, se tulostaa:

```Clojure
Clojure
```

Tämä ottaa alimerkkijonon indeksistä 4 indeksiin 13.

## Syvällisemmin:

Vaikka alimerkkijonot ovat olennainen osa monia ohjelmointikieliä, Clojuren `subs`-funktio on peräisin Javan String-luokan `substring`-metodista. Lisäksi sen sijaan, että käyttäisit `subs`:ia, voit käyttää `take`, `drop`, `split-at` -funktioita joitain skenaarioita varten. Niiden käyttäminen saattaa tarjota paremman suorituskyvyn, sillä ne ovat laiska evaluointeja.

Clojuren `subs`-implementointi käyttää Javan `substring`-metodia. Yksi tärkeä pointti muistaa on, että start- ja end-indeksi on 0-pohjainen ja end-indeksi on eksklusiivinen. Clojuren koodissa, `subs` ottaa kaksi parametria: start- ja end-indeksit.

## Katso Myös:

1. [Clojuren Dokumentaatio](https://clojuredocs.org/clojure.core/subs): Syvä sukellus `subs`-funktion käyttöön Clojuressa.
3. [Clojuren Sydän](https://www.youtube.com/watch?v=xguCXp0P1zE): Hyvä video, joka kattaa Clojuren Collection APIn, mukaan lukien merkkijonot.