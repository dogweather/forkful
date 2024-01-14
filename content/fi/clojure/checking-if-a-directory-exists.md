---
title:    "Clojure: Tarkistetaan, onko kansio olemassa."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat tarkistaa, onko hakemisto olemassa Clojure-ohjelmassa. Yksi yleisimmistä syistä on, että haluat välttää virheilmoitukset, jos yrität käsitellä hakemistoa, joka ei ole olemassa. 

## Näin teet sen

Voit tarkistaa hakemiston olemassaolon käyttämällä Clojuren `clojure.java.io/file` -funktiota. Tämä funktio palauttaa tiedoston olemassaolosta kertovan boolean-arvon.

```Clojure
(def hakemisto (clojure.java.io/file "polku/hakemistoon"))
(.exists hakemisto)
; => true
```

## Syvä Sukellus

Kun käsittelet hakemistoja, saattaa olla hyödyllistä tietää, että Clojurella on myös `clojure.java.io/directory?` -funktio, joka tarkistaa, onko annettu tiedosto hakemisto vai ei. Voit myös käyttää `clojure.java.io/list-files` -funktiota saadaksesi listan hakemistossa olevista tiedostoista.

Voit myös käyttää `clojure.java.io/make-directory` -funktiota luodaksesi uuden hakemiston, jos sitä ei vielä ole olemassa.

```Clojure
(def uusi-hakemisto (clojure.java.io/file "polku/uusi-hakemisto"))
(clojure.java.io/make-directory uusi-hakemisto)
; => polku/uusi-hakemisto

(def olemassaoleva-hakemisto (clojure.java.io/file "polku/olemassaoleva-hakemisto"))
(clojure.java.io/make-directory olemassaoleva-hakemisto)
; => salaisuus, hakemisto on jo olemassa!

```

## Katso Myös

- [Clojure.java.io -dokumentaatio](https://clojuredocs.org/clojure.java.io)
- [Clojure Cheatsheet - APIdock](https://apidock.com/ruby/Cheatsheets/Clojure-Cheatsheet)