---
title:    "Haskell: Satunnaisten lukujen generointi"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi generoida satunnaislukuja ohjelmoinnissa? Satunnaislukujen generointi on hyödyllistä, kun halutaan simuloita tietokoneella tapahtumia tai tehdä satunnaispohjaisia päätöksiä.

## Miten

Satunnaislukujen generoiminen Haskellissa on helppoa ja tehokasta. Se tapahtuu käyttämällä Random-kirjastoa, joka tarjoaa monia valmiita työkaluja satunnaislukujen luomiseen.

```Haskell
import System.Random

-- Generoidaan satunnainen kokonaisluku välillä 1-10
generateInt :: IO Int
generateInt = randomRIO (1, 10)

-- Generoidaan satunnainen liukuluku välillä 0.0-1.0
generateFloat :: IO Float
generateFloat = randomIO

-- Generoidaan lista satunnaisia liukulukuja välillä 1.0-100.0
generateList :: IO [Float]
generateList = randomRs (1.0, 100.0)

-- Generoidaan satunnainen merkkijono
generateString :: IO String
generateString = randomIO
```

Ohjelman suorittaminen tuottaa seuraavanlaisen tulosteen:

```
5
0.45825222
[14.715063,82.32594,44.826183,67.21938,75.008675,88.47541,38.569054,17.658438,5.645938,46.83128]
"Khrb92sL"
```

## Syvällinen sukellus

Satunnaislukujen generointi tietokoneella perustuu matemaattisiin algoritmeihin, joiden avulla voidaan luoda lukuja, joilla ei ole selkeää tai ennustettavaa järjestystä. Näitä algoritmeja kutsutaan myös pseudosatunnaislukugeneraattoreiksi, koska niiden tuottamat luvut ovat matemaattisesti määriteltyjä, mutta ne käyttäytyvät hyvin satunnaisilta.

Haskellin Random-kirjasto käyttää Mersenne Twister -algoritmia satunnaislukujen generoimiseen. Tämä on yksi suosituimmista ja tehokkaimmista algoritmeista, jota käytetään ohjelmoinnissa.

On myös tärkeää ymmärtää, että satunnaislukugeneraattorit eivät todellisuudessa tuota täysin satunnaisia lukuja. Ne on suunniteltu palauttamaan samat luvut, jos niiden annetaan sama aloitusarvo. Tämän vuoksi on tärkeää huolehtia siitä, että aloitusarvo vaihtelee jokaisella kerralla, kun satunnaislukugeneraattoria kutsutaan.

## Katso myös

- [Haskellin Random-kirjaston dokumentaatio](https://hackage.haskell.org/package/random)
- [Mersenne Twister -algoritmi](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Satunnaislukugeneraattori](https://fi.wikipedia.org/wiki/Satunnaislukugeneraattori)