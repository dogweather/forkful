---
title:                "Satunnaislukujen generointi"
date:                  2024-01-20T17:49:27.640421-07:00
model:                 gpt-4-1106-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? [Mitä & Miksi?]
Random-lukujen generointi luo ennustamattomia arvoja. Ohjelmoijat tarvitsevat näitä simulointeihin, pelilogiikkaan ja turvalliseen salaukseen.

## How to: [Kuinka:]
```Haskell
import System.Random (randomRIO)

-- Arvojen generointi väliltä 1-10
randomExample :: IO Int
randomExample = randomRIO (1, 10)

main :: IO ()
main = do
    arvo <- randomExample
    putStrLn ("Generoitu random-luku on: " ++ show arvo)
```

Esimerkkiajo:
```
Generoitu random-luku on: 7
```

## Deep Dive [Syväluotaus]
Historiallisesti, Haskellin random-moduulit ovat kehittyneet tarjoamaan parempaa suorituskykyä ja turvallisempia tuloksia. Vaihtoehtoja sisältävät `System.Random` ja kolmannen osapuolen kirjastot, kuten `random-fu`. Käyttöympäristöön vaikuttaa esimerkiksi haluatko puhdasta toiminnallisuutta (`Pure`) vai sivuvaikutuksia (`IO`). `randomRIO` on `IO`-toiminto, joka käyttää globaalia satunnaisten lukujen siementä turvallisuuden ja mukavuuden parantamiseksi.

## See Also [Katso Myös]
- Haskellin dokumentaatio `System.Random` -moduulista: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
- `random-fu` kirjaston dokumentaatio: http://hackage.haskell.org/package/random-fu
- Wiki-artikkeli Haskellista ja satunnaislukujen generoinnista: https://wiki.haskell.org/Random_number_generation