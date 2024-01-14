---
title:                "Haskell: Alaryhmien erottelu"
simple_title:         "Alaryhmien erottelu"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi: Miksi käyttää substringin erottelua?

Substringien erottaminen on hyödyllistä kun halutaan hakea tietoa tietyistä osioista merkkijonosta tai verrata eri merkkijonoja osittain. Tämä voi olla hyödyllistä esimerkiksi tekstitiedostojen käsittelyssä.

## How To: Miten käyttää substringin erottelua

Substringin erottelu on helppoa Haskell-kielen avulla. Käytämme siihen `take` ja `drop` funktioita, jotka ottavat parametreikseen halutun merkkijonon sekä aloitus- ja lopetusindeksit.

```Haskell
take 4 "Tervetuloa!" -- palauttaa "Terv"
drop 9 "Tervetuloa!" -- palauttaa "a!"
```

Molemmat funktiot palauttavat uuden merkkijonon, joka koostuu alkuperäisestä merkkijonosta määritetyiltä indekseiltä. Voit myös yhdistää nämä funktiot ja saada tarkemman substringin haluamiltasi kohdilta.

```Haskell
take 4 (drop 9 "Tervetuloa!") -- palauttaa "!a"
```

## Deep Dive: Syvempää tietoa substringin erottelusta

Substringien erottelu voi olla myös hyödyllistä kun halutaan käsitellä merkkijonoja listoina. Voimme esimerkiksi muuttaa merkkijonon listaksi käyttämällä `words` funktiota, joka erottaa sanat välilyöntien kohdalta.

```Haskell
words "Oletko nähnyt supersankareita?" -- palauttaa ["Oletko", "nähnyt", "supersankareita?"]
```

Tämän jälkeen voimme käsitellä sanoja listana ja esimerkiksi etsiä tiettyjä sanoja tai tarkastella sanojen pituuksia. Voimme myös yhdistää `take` ja `drop` funktiot listan kanssa ja käsitellä haluamaamme osaa merkkijonosta.

## Katso myös:

- [Haskell substringin erottelu](https://www.w3schools.com/haskell/show_code.asp?filename=demo_substr)
- [Haskell stringien käsittely](https://wiki.haskell.org/Strings) 
- [Substringien erottelun käyttö esimerkiksi tekstitiedostoissa](https://www.sis.uta.fi/~hxiao/SV/SV2011/lecture/IO.case2.substrings.pdf)