---
title:                "Haskell: Tiedostotekstin kirjoittaminen"
simple_title:         "Tiedostotekstin kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Yksi keino ilmaista ohjelmointilogiikkaa on tekstina. Kirjoittamalla Haskell-ohjelma tekstilukijalla, voi kehittäjä ymmärtää paremmin omaa koodiaan ja helpommin selittää sitä muille.

# Kuinka kirjoittaa tekstilukija Haskellilla

Kirjoittaminen tekstilukijalla on yksinkertaista Haskellilla. Ensiksi, luodaan "text" niminen funktio, joka ottaa sisään halutun tekstin ja palauttaa sen tulosteena. Esimerkiksi:

```Haskell
text "Hei maailma!"
```
tulostaa seuraavan tekstin:

```
Hei maailma!
```
Voi myös luoda funktioita, jotka käsittelevät ja muokkaavat tekstiä eri tavoin. Esimerkiksi:

```Haskell
capitalize s = map Char.toUpper s
```
tämä funktio muuttaa annetun tekstin käyttämällä "map" funktiota ja "Char.toUpper" funktiota, joka muuttaa allekirjoille isot kirjaimiksi. Nyt kun käytetään tekstifunktiota:

```Haskell
capitalize "Hei!"
```
tulosteena on:

```
HEI!
```

# Syvemmälle tekstilukijan kirjoittamiseen

Tekstilukijan kirjoittaminen voi auttaa kehittäjää ymmärtämään koodinsa loogista rakennetta ja logiikkaa paremmin. Se myös helpottaa ohjelman selittämistä ja muille kehittäjille jakamista. Tekstilukija voi sisältää funktioita, jotka tarkistavat esimerkiksi jos tekstissä esiintyy tiettyjä merkkejä tai palauttavat tiettyjä tulosteita, jos tiettyjä ehtoja täyttyy.

# Katso myös

- [Blogikirjoitus: "Kuinka kirjoittaa tekstilukija Haskellilla"](https://www.haskell.org/documentation)
- [Haskellin oppikirja](http://www.cs.nott.ac.uk/~pszgmh/pih.html)
- [Haskell-ohjelmointiympäristö](https://www.haskell.org/ghc/)