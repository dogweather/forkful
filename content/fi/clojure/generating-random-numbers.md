---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:06.945667-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Satunnaislukujen generointi on prosessi uusien, arvaamattomien numeroiden luomiseksi. Ohjelmoijat käyttävät niitä simulaatioissa, peleissä ja turvallisuudessa, varmistaakseen järjestelmien ennakoimattomuuden ja reiluuden.

## How to: (Kuinka tehdään:)
```Clojure
;; Arvon satunnaislukuja
(rand) ; väliltä 0.0 - 1.0
(rand-int 10) ; kokonaisluku väliltä 0 - 9

;; Arvon satunnaisia numeroita tietyn kokoelmasta
(rand-nth [10 20 30 40 50]) ; esim. 30
```
Sample output (esimerkkitulo):
```Clojure
0.7090036868539567
7
30
```

## Deep Dive (Sukellus syvyyksiin):
Satunnaislukujen generointi on vanha konsepti, yhtä vanha kuin matematiikka itse. Algoritmeja, kuten LCG (Linear Congruential Generators), on käytetty vuosikymmenten ajan. Clojuressa `(rand)` ja `(rand-int n)` käyttävät Java-kirjastoa, joka takaa hyvän satunnaisuuden.

Vaihtoehtoisia menetelmiä ovat tiheysfunktioiden käyttö `(rand-norm)` ja sekvenssien sekottaminen `(shuffle)`. Suorituskykyä ja turvallisuutta arvioidessa kannattaa tutkia krypto-graafisesti turvalliset RNG:t, kuten Java:n `SecureRandom`.

Clojuren `(rand)` toimii lazy-seq:jen kanssa. Se tarkoittaa, etteivät kaikki luvut generoidu kerralla, mikä säästää muistia ja tehostaa ohjelmien suoritusta.

## See Also (Katso myös):
- ClojureDocs, kattava opas Clojure-funktioille: [ClojureDocs](https://clojuredocs.org)
- “Clojure for the Brave and True”, luku satunnaisuudesta: [Brave Clojure](https://www.braveclojure.com)
- Java SecureRandom dokumentaatio: [Oracle SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
