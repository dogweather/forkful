---
title:    "Haskell: Tekstin hakeminen ja korvaaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku saattaisi haluta etsiä ja korvata tekstiä ohjelmointitehtävissä. Ehkä haluat muuttaa useita samanlaisia sanoja koodikappaleessa tai korjata kirjoitusvirheitä nopeasti. Haskellilla tämä onnistuu helposti ja tehokkaasti.

## Kuinka

Haskellissa voit käyttää funktiota nimeltä `replace` ja antaa sille parametreina etsittävän merkkijonon, korvaavan merkkijonon ja alkuperäisen merkkijonon. Esimerkiksi, jos haluat korvata kaikki kissat koirilla, voit kirjoittaa `replace "kissat" "koirat" "Tänään näin paljon kissoja."`. Tämä palauttaa uuden merkkijonon "Tänään näin paljon koiria.".

Voit myös käyttää regex ilmaisuja `replaceRegex` funktion avulla. Tällä tavalla voit etsiä ja korvata monimutkaisia sanoja tai lausekkeita. Esimerkiksi, jos haluat korvata kaikki numerot merkkijonossa nollalla, voit kirjoittaa `replaceRegex "[0-9]+" "0" "1, 2, 3, 4, 5"`. Tämä palauttaa "0, 0, 0, 0, 0".

## Syväsukellus

Haskellilla on monia muita hyödyllisiä funktioita, jotka ovat avuksi etsittäessä ja korvattaessa tekstiä. Joitakin hyödyllisiä ovat `map`, `filter` ja `concat`. Voit myös käyttää algebrallisia tyyppiluokkia kuten `Monoid` ja `Semigroup` helpottamaan manipulointia tekstiä.

On myös tärkeää muistaa, että Haskellissa merkkijonot ovat muuttumattomia eli funktiot eivät muuta alkuperäistä merkkijonoa vaan palauttavat uuden muutetun version. Tämä tekee Haskellista turvallisen ja luotettavan kielen tekstin käsittelyyn.

## Katso myös

- [Haskellin dokumentaatio merkkijonojen muokkaamiseen](https://www.haskell.org/hoogle/?hoogle=replace)
- [Hakusanaopas regex ilmaisuihin Haskellissa](http://www.cis.upenn.edu/~cis194/fall14/solutions/07%20Parsec.html)
- [Artikkeli merkkijonojen muuttumattomuudesta Haskellissa](https://wiki.haskell.org/Monomorphism_restriction)