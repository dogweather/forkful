---
title:                "Sattumanvaraisten lukujen luominen"
html_title:           "Clojure: Sattumanvaraisten lukujen luominen"
simple_title:         "Sattumanvaraisten lukujen luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

Mikä & Miksi?

Satunnaislukujen generoiminen on prosessi, jossa tietokone tuottaa sattumanvaraisia numeroita. Tämä on tärkeä työkalu monille ohjelmoijille, sillä se auttaa heitä muun muassa simuloimaan sattumanvaraisia tapahtumia ja testaamaan algoritmejaan.

Kuinka tehdä?

###### Clojure Satunnaislukujen generoiminen

```Clojure
;; Tuottaa yhden satunnaisen numeron väliltä 1-10
(rand-int 10)

;; Tuottaa yhden satunnaisen liukuluvun väliltä 0-1
(rand)

;; Tuottaa yhden satunnaisen merkin ASCII-taulukosta
(rand-nth "abcdefg")

;; Tuottaa satunnaisen elementin koko listasta
(rand-nth [1 2 3 4 5])

;; Tuottaa satunnaisen numeron jokaiselle listan elementille
(map (fn [x] (* x (rand-int 10))) [1 2 3 4])

```

Esimerkkituloste:

```
6
0.729503470161935
c
3
(5 8 6 12)
```

Syväsukellus

Satunnaislukujen generointi on ollut tärkeä osa tietokoneiden toiminnassa jo varhaisista vuosista lähtien. Algoritmit ovat kehittyneet vuosien saatossa, ja nykyään voidaan tuottaa todella lähellä oikeita sattumanvaraisia lukuja.

On kuitenkin hyvä huomata, että satunnaisuus tietokoneissa perustuu aina matemaattisiin algoritmeihin, ja siksi generoidut numerot eivät ole täysin sattumanvaraisia. On myös olemassa muita tapoja generoida satunnaisia lukuja, kuten esimerkiksi käyttämällä fyysisiä tapahtumia, kuten hiiren liikkeitä.

Katso myös

Clojure:n virallinen dokumentaatio satunnaislukujen generoinnista: https://clojuredocs.org/clojure.core/rand

Tietoa satunnaislukujen generoinnista ja sen käytöstä simuloinnissa: https://en.wikipedia.org/wiki/Random_number_generation