---
title:    "Clojure: Tekstitiedoston kirjoittaminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa tekstitiedostoa? Tekstiedostot ovat erittäin hyödyllisiä työkaluja, kun haluat tallentaa tietoa tekstimuodossa. Ne ovat myös hyvä tapa tallentaa ja jakaa koodia, jotta sen muokkaaminen ja ylläpitäminen olisi helpompaa.

## Miten

```Clojure
;; Luodaan uusi tiedosto nimeltä "tekstitiedosto.txt"
(def tiedosto (io/file "tekstitiedosto.txt"))

;; Kirjoitetaan tekstiä tiedostoon
(with-open [stream (io/writer tiedosto)]
    (.write stream "Tämä on tekstiä!"))

;; Suljetaan tiedosto
(.close stream)
```

Kun avaat "tekstitiedosto.txt" tiedoston, näet sen sisältävän tekstin "Tämä on tekstiä!". Voit myös käyttää `(.writeLine stream "Tämä on eri rivi!")` lisätäksesi tekstin eri riveille.

## Syvällinen sukellus

Tekstitiedoston luominen ja kirjoittaminen on erittäin hyödyllistä, mutta on tärkeää muistaa myös tiedoston sulkeminen käytön jälkeen. Tämä estää mahdolliset muutokset tiedostoon ja vapauttaa resursseja.

Voit myös käyttää `(.flush stream)` ennen tiedoston sulkemista, jotta kaikki voimassa olevat muutokset kirjoitetaan tiedostoon.

## Katso myös

- [Clojure io -kirjasto](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Tekstitiedoston kirjoittaminen ohjeet](https://clojure.org/guides/io)