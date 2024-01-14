---
title:    "Clojure: Regulaarilausekkeiden käyttö"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää tavallisia ilmauksia?

Tavalliset ilmaukset ovat erittäin hyödyllisiä työkaluja tiedonkäsittelyyn ja tekstinmuokkaukseen Clojure-ohjelmoinnissa. Niiden avulla voit löytää ja muokata tiettyjä tekstin osia, jotka noudattavat tiettyä kaavaa tai mallia. Tämä säästää aikaa ja vaivaa manuaalisen tiedonkäsittelyn sijaan.

## Miten käyttää tavallisia ilmauksia?

Tavallisia ilmauksia voi käyttää Clojure-kielen `re-find` ja `re-matcher` -toimintojen avulla. Voit etsiä haluamasi kaavan tai mallin tekstin joukosta ja palauttaa tuloksen haluamassasi muodossa. Esimerkiksi, jos haluat etsiä kaikki numerot tekstistä ja palauttaa ne listana, voit käyttää seuraavaa koodia:

```Clojure
(def teksti "Tänään on ensimmäinen helmikuuta ja kello on 12:30.")

(re-find #"[0-9]+" teksti)
=> ("1" "0" "12" "30")
```
Haluamme käyttää ilmestyspaikkaa (character class) `[0-9]+`, joka etsii kaikki numerot tekstistä. Käytämme myös `re-find`-funktiota löytääksemme kaikki täsmäävät ilmaukset ja palauttaaksemme ne listassa.

## Syvemmälle tavallisten ilmausten käyttöön

Tavallisia ilmauksia käyttäessä on tärkeää ymmärtää erilaisia ilmausten merkkejä ja niiden merkitys. Esimerkiksi `+` merkki ilmaisee, että edellinen ilmaus voi esiintyä useamman kerran, kun taas `*` merkki ilmaisee, että edellinen ilmaus voi olla tyhjä tai esiintyä useamman kerran. Voit löytää täydellisen listan ilmausten käyttötavoista ja merkityksistä Clojure:n dokumentaatiosta.

On myös hyvä muistaa, että tavalliset ilmaukset ovat melko tehokkaita, mutta samalla myös monimutkaisia. Niiden kanssa työskennellessä kannattaa olla kärsivällinen ja kokeilla erilaisia ilmauksia, kunnes löytää sopivan.

## Katso myös

- [Clojure:n dokomentaatio tavallisista ilmauksista](https://clojure.org/api/cheatsheet)
- [Regexr - Työkalu tavallisten ilmausten testaamiseen ja luomiseen](https://regexr.com/)