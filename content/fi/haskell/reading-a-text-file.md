---
title:    "Haskell: Tiedoston lukeminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi lukea tekstitiedostoja?

Monet ohjelmat ja sovellukset käsittelevät suuria määriä tietoa, ja usein tämä tieto tallennetaan tekstitiedostoihin. Tekstitiedostoja voidaan lukea ja käsitellä eri ohjelmointikielillä, mukaan lukien Haskell. Tässä blogikirjoituksessa opimme, kuinka voit lukea ja käyttää tekstitiedostoja Haskellissa.

## Miten tehdä se Haskellilla?

Haskellissa tekstitiedoston lukeminen on melko yksinkertaista. Käytämme `readFile`-funktiota, joka lukee tiedoston ja palauttaa sen sisällön merkkijonona.

```Haskell
-- Avataan tiedosto ja luetaan sen sisältö
tiedosto <- readFile "tiedostonimi.txt"
-- Tulostetaan tiedoston sisältö konsoliin
putStrLn tiedosto

-- Tulostaa: Tämä on tekstitiedosto.
```

Kuten näemme, `putStrLn`-funktio tulostaa tiedoston sisällön konsoliin. Voimme myös tallentaa tiedoston sisällön muuttujaan ja käyttää sitä muilla tavoilla.

```Haskell
-- Tallennetaan tiedoston sisältö muuttujaan
sisalto <- readFile "tiedostonimi.txt"
-- Etsitään ja tulostetaan tietty sana tiedoston sisällöstä
let sana = "tekstitiedosto."
putStrLn $ "Tiedostossa löytyi sana " ++ sana

-- Tulostaa: Tiedostossa löytyi sana tekstitiedosto.
```

Voimme myös käsitellä tiedoston sisältöä muilla tavoilla, kuten esimerkiksi muuttaa sen listaksi merkkijonojen sijaan. Alla olevassa esimerkissä muutamme tiedoston rivit listaksi ja tulostamme sen sisällön.

```Haskell
-- Tallennetaan tiedoston sisältö muuttujaan
sisalto <- readFile "tiedostonimi.txt"
-- Jaetaan tiedoston sisältö rivit listaksi
let rivit = lines sisalto
-- Tulostetaan listan sisältö konsoliin
putStrLn $ "Listassa on " ++ show (length rivit) ++ " riviä."

-- Tulostaa: Listassa on 1 riviä.
```

## Syvällinen sukellus

Vaikka `readFile`-funktio onkin helppokäyttöinen ja kätevä, on hyvä tietää myös muita tapoja lukea tiedostoja Haskellissa. Voimme käyttää `withFile`-funktiota, joka avaa tiedoston ja suorittaa määritellyn toiminnon sen sisällölle.

```Haskell
-- Avataan tiedosto ja suoritetaan toiminto sen sisällölle
withFile "tiedostonimi.txt" ReadMode $ \tiedosto -> do
    sisalto <- hGetContents tiedosto
    putStrLn sisalto

-- Tulostaa: Voit myös lukea tekstitiedostoja withFile-funktiolla!
```

Voimme myös määrittää tiedoston avaustilaksi `WriteMode`, jolloin voimme kirjoittaa tiedoston sisältöä.

```Haskell
-- Avataan tiedosto kirjoittamista varten
withFile "uusitiedosto.txt" WriteMode $ \tiedosto -> do
    -- Kirjoitetaan tiedoston sisältöä
    hPutStrLn tiedosto "Kirjoitetaan uuteen tiedostoon!"
    hPutStrLn tiedosto "Tämä on uusi rivi."
```

## Katso myös

- [Haskellin virallinen dokumentaatio tiedoston lukemisesta](https://www.haskell.org/tutorial/io.html#reading-files)
- [Learn You a Haskell for Great Good! -kirjan luku tiedostoista](http://learnyouahaskell.com/input-and-output#files-and-streams)