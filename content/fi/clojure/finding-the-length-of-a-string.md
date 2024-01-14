---
title:                "Clojure: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monilla kielillä, mukaan lukien Clojure, on sisäänrakennettu funktio, jolla voi laskea merkkijonon pituuden. Tämä voi olla hyödyllistä monissa ohjelmoinnin sovelluksissa, kuten puhelinnumeroita tai sähköpostiosoitteita käsiteltäessä.

## Miten

Käytämme tätä Artikkelia mukana Clojure versio 1.9.0. Seuraavassa esimerkissä käytämme `count` -funktiota selvittämään merkkijonon pituuden ja tulostamme sen konsoliin:

```Clojure
(def s "Tämä on testimerkkijono")
(count s)

;; Output: 23
```

Yllä olevassa esimerkissä määritellään `s` -muuttuja, johon tallennetaan testimerkkijono. Sen jälkeen `count` -funktiota käytetään `s` -muuttujalle, ja se palauttaa merkkijonon pituuden, joka on 23.

Voimme myös käyttää `count` -funktiota suoraan merkkijonojen kanssa ilman muuttujaa:

```Clojure
(count "Tämä on toinen testimerkkijono")

;; Output: 27
```

Huomaa, että välilyönnit lasketaan myös merkkien joukkoon, joten merkkijonon pituus voi poiketa merkkien määrästä.

## Syvällisempi tarkastelu

`count` -funktio käyttää todellisuudessa `seq` -funktiota selvittääkseen merkkijonon pituuden. Tämä tarkoittaa sitä, että kaikki kokoelman tyyppiset arvot ovat kelvollisia parametreja `count` -funktiolle, mukaan lukien listat, vektorit ja mapit.

```Clojure
(def test-list [1 2 3 4])
(count test-list)

;; Output: 4
```

On myös tärkeää huomata, että `count` -funktio ei tarkista vain ensimmäistä tasoa kokoelmassa, vaan se laskee myös sisäkkäisesti olevien kokoelmien arvojen määrät.

```Clojure
(count [[1 2 3] [4 5 6] [7 8 9]])

;; Output: 3
```

## Katso myös

- [Clojure - Dokumentaatio](https://clojure.org/documentation)
- [Clojuredocs - count](https://clojuredocs.org/clojure.core/count)