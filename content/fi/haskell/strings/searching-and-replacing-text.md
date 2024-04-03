---
date: 2024-01-20 17:58:13.914510-07:00
description: "Tekstin etsint\xE4 ja korvaaminen auttaa l\xF6yt\xE4m\xE4\xE4n ja muokkaamaan\
  \ koodissa tai tekstitiedostoissa olevia merkkijonoja. Ohjelmoijat k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 toimintoa\u2026"
lastmod: '2024-03-13T22:44:56.601255-06:00'
model: gpt-4-1106-preview
summary: "Tekstin etsint\xE4 ja korvaaminen auttaa l\xF6yt\xE4m\xE4\xE4n ja muokkaamaan\
  \ koodissa tai tekstitiedostoissa olevia merkkijonoja."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to:
```Haskell
import Data.List (isInfixOf)

-- Etsi tekstiä listasta ja korvaa uudella tekstillä
replaceText :: String -> String -> String -> String
replaceText old new = unwords . map (replaceWord old new) . words
  where
    replaceWord o n word = if o `isInfixOf` word then n else word

main :: IO ()
main = do
    let originalText = "Tervetuloa Haskellin ihmeelliseen maailmaan!"
    let newText = replaceText "ihmeelliseen" "hauskaan" originalText
    putStrLn newText
```

Esimerkkitulostus:
```
Tervetuloa Haskellin hauskaan maailmaan!
```

## Deep Dive
Haskellissa tekstinkäsittely on funktionaalista. Se tarkoittaa, että tekstin etsiminen ja korvaaminen tapahtuu ilman muuttuvia tiloja. Historiallisesti tämä lähestymistapa juontaa juurensa alkujaan LISP-kielestä, joka vaikutti funktionaalisen ohjelmoinnin kehitykseen.

Vaihtoehtoisesti voit käyttää regex-kirjastoa monimutkaisempiin hakuihin ja korvauksiin (esim. `Text.Regex.TDFA`). Tämän kirjaston avulla voit käyttää säännöllisiä lausekkeita tehokkaampaan tekstinkäsittelyyn.

Haskell toteuttaa tekstinkorvauksen immutaabelilla tavalla, mikä tarkoittaa, että alkuperäinen teksti pysyy muuttumattomana. Tämä on yksi monista Haskellin ominaisuuksista, joka auttaa välttämään sivuvaikutuksia ja tekee koodista turvallisempaa.

## See Also
- Haskellin dokumentaatio: [https://www.haskell.org/documentation/](https://www.haskell.org/documentation/)
- "Learn You a Haskell for Great Good!" -kirja aloittelijoille: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- `regex-tdfa` kirjasto: [https://hackage.haskell.org/package/regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
