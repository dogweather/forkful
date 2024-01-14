---
title:    "Clojure: Tiedoston lukeminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi?
Tekstitiedostojen lukeminen on oleellinen osa monia ohjelmointiprojekteja. Se mahdollistaa datan käsittelyn ja analysoinnin, mikä on tärkeä osa monia sovelluksia ja työkaluja.

## Miten?
Text-tiedoston lukeminen on yksiä yksinkertaisimmista toiminnoista Clojure-ohjelmoinnissa. Voit käyttää `(slurp "tiedostonimi")`-funktiota, joka palauttaa funktion sisältämän tekstin merkkijonona. Katso alla olevasta esimerkistä:
```Clojure
(slurp "tiedosto.txt")
```
Tämä koodi lukee "tiedosto.txt"-nimisen tiedoston ja palauttaa sen sisällön tekstimuodossa.

Slurp-funktio toimii myös verkkosivujen ja muiden tiedostojen kanssa. Voit siis käyttää sitä monipuolisesti datan lukemiseen ohjelmassasi.

## Syvemmälle
On tärkeää huomata, että slurp-funktio palauttaa tekstin merkkijonona, joka sisältää kaikki tiedoston rivit sellaisenaan. Tätä tekstiä voi sitten käsitellä ja pilkkoa haluamallasi tavalla.

Voit myös käyttää muita Clojure-kirjastoja, kuten [clojure.data.csv](https://github.com/clojure/data.csv) tai [data.xml](https://github.com/clojure/data.xml), jos haluat lukea ja käsitellä tiedoston sisältöä tiettyjen formaattien mukaan.

## Katso myös
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)
- [Clojure Made Simple](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
- [Clojure Bridge - Suomenkielinen aloittelijoiden kurssi](https://www.clojurebridge.org/community/translations/fi)