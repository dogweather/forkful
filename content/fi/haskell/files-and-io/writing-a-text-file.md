---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:13.309995-07:00
description: "Kuinka: Haskellin standard Prelude tarjoaa perustason tuen tiedostoihin\
  \ kirjoittamiselle `writeFile` ja `appendFile` funktioiden avulla `System.IO`\u2026"
lastmod: '2024-03-13T22:44:56.632812-06:00'
model: gpt-4-0125-preview
summary: Haskellin standard Prelude tarjoaa perustason tuen tiedostoihin kirjoittamiselle
  `writeFile` ja `appendFile` funktioiden avulla `System.IO` moduulista.
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Haskellin standard Prelude tarjoaa perustason tuen tiedostoihin kirjoittamiselle `writeFile` ja `appendFile` funktioiden avulla `System.IO` moduulista. Tässä on perusesimerkki uuden tiedoston luomisesta (tai olemassa olevan ylikirjoittamisesta) ja sitten tekstin lisäämisestä tiedostoon.

```haskell
import System.IO

-- Kirjoittaminen tiedostoon, ylikirjoittaen jos se on olemassa
main :: IO ()
main = do
  writeFile "example.txt" "Tämä on ensimmäinen rivi.\n"
  appendFile "example.txt" "Tämä on toinen rivi.\n"
```

Kun ajat tämän ohjelman, se luo (tai tyhjentää) `example.txt` tiedoston ja kirjoittaa "Tämä on ensimmäinen rivi." ja sen jälkeen "Tämä on toinen rivi." seuraavalle riville.

Kehittyneempään tiedostonkäsittelyyn Haskell-ohjelmoijat kääntyvät usein `text` paketin puoleen tehokkaan merkkijonojen käsittelyn vuoksi ja `bytestring` paketin puoleen binääridatan käsittelyssä. Näin käytät `text` pakettia tiedostojen IO:ssa:

Ensiksi, sinun täytyy lisätä `text` projektiisi riippuvuuksien joukkoon. Sen jälkeen, voit käyttää sitä seuraavasti:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Kirjoittaminen tiedostoon käyttäen text-pakettia
main :: IO ()
main = do
  let content = T.pack "Käyttäen text-pakettia paremman suorituskyvyn saavuttamiseksi.\n"
  TIO.writeFile "textExample.txt" content
  TIO.appendFile "textExample.txt" $ T.pack "Lisäten toista riviä.\n"
```

Tässä pätkässä, `T.pack` muuntaa tavallisen `String`:n `Text` tyyppiseksi, mikä on tehokkaampaa. `TIO.writeFile` ja `TIO.appendFile` ovat `text` vastineet tiedostoihin kirjoittamiselle ja tiedostoon liittämiselle vastaavasti.

Tämän koodin ajaminen tuottaa tiedoston nimeltä `textExample.txt` kahdella tekstirivillä, osoittaen sekä luomis- että liittämiskyvykkyydet käyttäen kehittynyttä `text` kirjastoa paremman suorituskyvyn ja kyvyn käsitellä Unicode-tekstiä varten.
