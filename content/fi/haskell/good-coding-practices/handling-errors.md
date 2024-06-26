---
date: 2024-01-26 00:53:41.238366-07:00
description: "Miten: Haskell k\xE4sittelee virheit\xE4 luotettavasti tyyppien kuten\
  \ `Maybe` ja `Either` avulla. T\xE4ss\xE4 nopea katsaus."
lastmod: '2024-03-13T22:44:56.622270-06:00'
model: gpt-4-1106-preview
summary: "Haskell k\xE4sittelee virheit\xE4 luotettavasti tyyppien kuten `Maybe` ja\
  \ `Either` avulla."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Miten:
Haskell käsittelee virheitä luotettavasti tyyppien kuten `Maybe` ja `Either` avulla. Tässä nopea katsaus:

```Haskell
safeDivide :: Integral a => a -> a -> Maybe a
safeDivide _ 0 = Nothing  -- Nollaan jakaminen ei käy, palautamme siis Nothing.
safeDivide x y = Just (x `div` y)  -- Muutoin kaikki on hyvin, palautamme tuloksen Just:ssa.

-- Katsotaanpa sitä toiminnassa:
example1 :: Maybe Int
example1 = safeDivide 10 2  -- Just 5

example2 :: Maybe Int
example2 = safeDivide 10 0  -- Nothing
```

Monimutkaisempaan virheenkäsittelyyn, `Either` tulee mukaan:

```Haskell
safeDivideEither :: Integral a => a -> a -> Either String a
safeDivideEither _ 0 = Left "Divide by zero error."  -- Tällä kertaa virhe sisältää viestin.
safeDivideEither x y = Right (x `div` y)

-- Ja käytössä:
example3 :: Either String Int
example3 = safeDivideEither 10 2  -- Right 5

example4 :: Either String Int
example4 = safeDivideEither 10 0  -- Left "Divide by zero error."
```

## Syväsukellus
Haskellassa virheenkäsittelyllä on vahvat juuret. Aikoinaan virheet saattoivat kaataa koko ohjelman—ei hauskaa. Haskelin tyyppijärjestelmä tarjoaa keinoja tehdä tästä paljon epätodennäköisempää. Meillä on `Maybe` ja `Either`, mutta muitakin, kuten `Exceptions` ja `IO` eri skenaarioihin.

`Maybe` on yksinkertainen: saat `Just` jotain, jos kaikki on hyvin, tai `Nothing`, jos ei ole. `Either` ottaa askeleen pidemmälle, mahdollistaen virhesanoman (`Left`) tai onnistuneen tuloksen (`Right`) palauttamisen.

Molemmat ovat puhtaita, mikä tarkoittaa, että ne eivät sekoitu ulkomaailmaan—iso juttu Haskellessa. Vältämme tarkistamattomien poikkeusten aiheuttamat ongelmat, joista jotkut muut kielet kärsivät.

Niille, jotka eivät ole tyytyväisiä `Maybe`n ja `Either`n kanssa, kirjastot kuten `Control.Exception` tarjoavat perinteisempää, imperatiivista virheenkäsittelyä poikkeusten kautta. Mutta niiden liiallinen käyttö voi monimutkaistaa asioita, joten yhteisö usein pysyttelee näissä tyypeissä.

## Katso Myös
Sukella syvemmälle:

- Haskelin omat dokumentit: [Haskell](https://haskell.org/documentation)
- Aloittelijoille sopiva: ["Learn You a Haskell for Great Good!"](http://learnyouahaskell.com/)
