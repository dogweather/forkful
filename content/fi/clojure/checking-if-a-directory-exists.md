---
title:                "Clojure: Tarkistetaan, onko hakemistoa olemassa."
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tutkia onko hakemisto olemassa

Hakemistojen tarkistaminen on tärkeä osa ohjelmoinnin arkea, sillä se auttaa varmistamaan, että tiedostojärjestelmässä on tarvittavat tiedostot saatavilla ennen kuin niihin viitataan. Tämä on erityisen tärkeää silloin, kun ohjelma käsittelee suuria määriä tiedostoja, jotta vältetään mahdolliset virheet ja ohjelman kaatumiset.

## Miten tehdä se Clojurella

Hakemiston olemassaolon tarkistaminen Clojurella on yksinkertaista. Voit käyttää `clojure.java.io/file` -funktiota luomaan tiedosto-olion halutulle polulle. Tämän jälkeen voit käyttää `(.exists file)` -funktiota tarkistaaksesi, onko tiedosto olemassa. Esimerkiksi:

```Clojure
(def olemassa (clojure.java.io/file "polku" "tiedostonimi"))
(.exists olemassa) ; palauttaa true tai false
```

Voit myös käyttää `clojure.java.io/file?` -funktiota, joka palauttaa boolean-arvon tiedoston olemassaolosta ilman erillistä tarkistusta. Esimerkiksi:

```Clojure
(def olemassa? (clojure.java.io/file? "polku" "tiedostonimi"))
```

## Syväsukellus

Hakemistojen tarkistaminen ei ole pelkästään tiedostojen olemassaolon varmistamista, vaan se auttaa myös virheiden käsittelyssä. Jos esimerkiksi tiettyä tiedostoa ei ole olemassa, ohjelma voi käsitellä tilanteen ja antaa virheen sijaan järkevämmän ilmoituksen käyttäjälle.

Hakemistojen tarkistaminen voi myös olla hyödyllistä, kun käsitellään hakemistojen sisältöä. Esimerkiksi voit tarkistaa, onko hakemisto tyhjä ennen sen sisällön käsittelyä.

## Katso myös

- [Clojure Doc: clojure.java.io/file](https://clojuredocs.org/clojure.java.io/file)
- [Clojure Doc: clojure.java.io/file?](https://clojuredocs.org/clojure.java.io/file%3F)