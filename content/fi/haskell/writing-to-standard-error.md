---
title:                "Standardivirheen kirjoittaminen"
html_title:           "Haskell: Standardivirheen kirjoittaminen"
simple_title:         "Standardivirheen kirjoittaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kirjoittaminen vakiotulosteeseen on tapa ilmoittaa tärkeitä viestejä ohjelman suorituksen aikana. Tämä on hyödyllistä esimerkiksi virheilmoituksissa tai tietojen tarkistamisessa, jotta ohjelman toiminnasta saadaan parempi yleiskuva. Ohjelmoijat käyttävät tätä tapaa varmistaakseen, että heidän koodinsa toimii niin kuin haluavat.

## Miten?
```Haskell
import System.IO

-- Kirjoita "Virhe!" vakiotulosteeseen
hPutStrLn stderr "Virhe!"

-- Kirjoita muuttujan arvo vakiotulosteeseen
let x = 42
hPutStrLn stderr ("x on " ++ show x)
```
Tällä koodilla saat tulosteen ```Virhe!``` sekä ```x on 42```.

## Syväsukellus
Standardi virhetulosteella on pitkä historia, ja se löytyy monista eri ohjelmointikielistä. Sen avulla koodin suorituksen aikaiset virheet ja varoitukset voidaan näyttää käyttäjälle selkeämmin ja ymmärrettävämmin. Joskus ohjelmoijat haluavat myös ohjata tulosteensa virhetulosteeseen, jotta he voivat erottaa sen muusta tulosteesta helpommin.

Jos et halua käyttää standardi virhetulostetta, voit luoda oman tulosteen käyttämällä esimerkiksi ```putStrLn``` funktiota, joka tulostaa viestin vakiotulosteeseen.

Tarkempi tieto kirjoittamisesta standoardi virhetulosteeseen löytyy Haskellin dokumentaatiosta.

## Katso myös
- [Haskellin dokumentaatio](https://www.haskell.org/documentation/)
- [System.IO-kirjasto](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)