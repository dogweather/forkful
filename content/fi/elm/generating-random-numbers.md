---
title:                "Satunnaisten numeroiden generointi"
html_title:           "Elm: Satunnaisten numeroiden generointi"
simple_title:         "Satunnaisten numeroiden generointi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

Mitä & Miksi?
Satunnaislukujen generointi tarkoittaa lukujen luomista, jotka eivät seuraa peräkkäistä järjestystä tai kaavaa. Tätä tehdään usein ohjelmoinnissa sattumanvaraisen tai arvaamattoman toiminnan luomiseksi.

Miksi ohjelmoijat tekevät sitä? Satunnaislukujen generointi on hyödyllistä esimerkiksi peleissä, simulaatioissa tai kaikissa ohjelmissa, jotka tarvitsevat sattumanvaraisen toiminnan elementin.

Miten teet sen:
```Elm
import Random

--generoi kokonaisluku välillä 1-10
Random.int 1 10
--output: 6
```

Voit myös käyttää seediä, jotta saat aina saman satunnaisluvun.
```Elm
Random.initialSeed 42
    |> Random.int 1 10
--output: 4
```

Syvemmälle:
Satunnaislukujen generointi ei ole uusi konsepti. Se oli ensimmäisen kerran esitelty vuonna 1946 Bell Labsin tutkijoiden julkaisemassa artikkelissa. Alternatiivina Elmille, voit käyttää muita kieliä, kuten JavaScript, joka sisältää myös satunnaislukujen generoinnin toiminnon.

Jos olet kiinnostunut tarkemmista yksityiskohdista, Elm käyttää satunnaislukujen luomiseen XorShift-algoritmia, joka on nopea ja luotettava tapa generoida satunnaislukuja.

Katso myös:
- [Elm Random -dokumentaatio](https://package.elm-lang.org/packages/elm/random/latest/)
- [Satunnaislukujen generointi JavaScriptillä](https://www.w3schools.com/js/js_random.asp)