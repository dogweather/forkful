---
title:                "Haskell: Tekstitiedoston lukeminen."
simple_title:         "Tekstitiedoston lukeminen."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet kiinnostunut Haskell-ohjelmoinnista tai yksinkertaisesti opettelet kieltä, tiedoston lukeminen on tärkeä taito, jota tarvitset monissa sovelluksissa. Teksti tiedostoja käytetään usein tietojen tallentamiseen ja jakamiseen, joten osaaminen niiden lukemisessa on erittäin hyödyllistä.

## Kuinka

Aloita avaamalla haluamasi tiedosto ```Using System.IO``` -kirjaston avulla. Jos haluat käyttää komentoriviltä päivitettävää tiedostoa, voit käyttää ```getContents```. Voit lukea tiedoston sisällön monella tavalla, kuten riviriviltä tai kokonaisuudessaan. Tässä on esimerkkejä:

```Haskell
import System.IO

main = do
  -- Avaa tiedosto ja tulosta sen sisältö riviriviltä
  handle <- openFile "tiedosto.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

  -- Lue tiedoston sisältö ja tulosta se kokonaisuudessaan
  contents <- readFile "tiedosto.txt"
  putStrLn contents
```

#### Tuloste:

```
Tämä on testitiedosto.
Rivi 2
Rivi 3
```

## Syväsyvennystä

Tiedostojen lukeminen tapahtuu monessa osassa. Ensinnäkin tiedosto avataan, sitten sen sisältö luetaan ja lopuksi tiedosto suljetaan. Tämä on tärkeää pitää mielessä, jotta tiedosto ei jää vahingossa auki ja aiheuta ongelmia. Lisäksi, kun tiedosto on avattu, sen sisältöä pystytään lukemaan eri tavoilla, kuten merkkeinä tai kokonaisina riveinä. Erilaiset rakenteet, kuten kaaviosolut tai puut, voivat auttaa tiedoston sisällön järjestelyssä ja käsittelyssä.

## Katso myös

- [Haskell: Tiedoston käsittely](https://wiki.haskell.org/Handling_files)
- [Haskell: Systeemi-moduuli (System.IO)](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Tiedoston käsittely Haskellissa: Näin onnistut ilman pettymyksiä](https://techinsight.io/tiedostonkasittely-haskelilla-nain-onnistut-ilman-pettymyksia/)