---
title:                "Haskell: Satunnaisten numeroiden luominen"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää satunnaislukujen generaattoria Haskell-ohjelmoinnissa? Satunnaisuus on tärkeä osa monia ohjelmia ja algoritmeja, koska se mahdollistaa ennustamattomuuden ja monipuolisuuden. Satunnaislukujen avulla voidaan esimerkiksi luoda erilaisia tietoaineistoja testausta varten tai simuloida todellisia tilanteita.

## Miten

Haskell tarjoaa valmiin kirjaston satunnaislukujen generointiin, nimeltään "System.Random". Ensimmäiseksi on tuotava kyseinen kirjasto moduuliksi ja määriteltävä random-luku generaattori.

```Haskell
import System.Random

-- Luodaan random-luku generaattori annetulla siemenarvolla
randomGen :: Int -> StdGen
randomGen seed = mkStdGen seed

-- Generoidaan 10 satunnaista lukua väliltä 1-100
randomNumbers :: StdGen -> [Int]
randomNumbers gen = take 10 $ randomRs (1,100) gen
```

Yllä olevassa koodiesimerkissä luodaan ensin random-luku generaattori annetulla siemenarvolla käyttäen "mkStdGen" funktiota. Sitten luodaan "randomNumbers" funktio, joka ottaa parametriksi generaattorin ja palauttaa listan 10 satunnaista lukua väliltä 1-100 käyttäen "randomRs" funktiota. Tässä tapauksessa funktio palauttaa aina saman listan, jos sitä kutsutaan samalla siemenarvolla.

## Syvemmälle

Haskellin "System.Random" kirjasto tarjoaa monia erilaisia funktioita ja työkaluja satunnaislukujen generointiin erilaisin rajoituksin ja tarkkuuksin. Esimerkiksi käyttämällä "split" funktiota, voidaan yhdestä generaattorista luoda kaksi uutta generaattoria, jotka tuottavat erilaisia satunnaislukuja.

```Haskell
import System.Random

-- Luodaan alkuperäinen random-luku generaattori
gen :: StdGen
gen = mkStdGen 1234

-- Jaetaan generaattori kahteen uuteen 
(gen1, gen2) = split gen

-- Generoidaan 5 satunnaista lukua ensimmäisellä generaattorilla
numbers1 = take 5 $ randomRs (1,10) gen1

-- Generoidaan 5 satunnaista lukua toisella generaattorilla
numbers2 = take 5 $ randomRs (1,10) gen2
```

Tässä tapauksessa "numbers1" ja "numbers2" listat ovat erilaiset, vaikka ne ovatkin generoitu samasta alkuperäisestä generaattorista.

## Katso myös

- [Haskellin random-kirjaston dokumentaatio](https://hackage.haskell.org/package/random)
- [Haskellin opetusmateriaali (suomeksi)](http://www.cs.helsinki.fi/u/glinden/HyOpinnot/Harjoitukset/haskell/hask_intro.php)
- [Haskellin perusteet (englanniksi)](https://www.haskell.org/tutorial/)