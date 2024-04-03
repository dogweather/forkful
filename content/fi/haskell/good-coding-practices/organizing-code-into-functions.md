---
date: 2024-01-26 01:10:21.757800-07:00
description: "Miten: T\xE4ss\xE4 on, miten voit kirjoittaa ja k\xE4ytt\xE4\xE4 funktioita\
  \ Haskellissa."
lastmod: '2024-03-13T22:44:56.620474-06:00'
model: gpt-4-1106-preview
summary: "T\xE4ss\xE4 on, miten voit kirjoittaa ja k\xE4ytt\xE4\xE4 funktioita Haskellissa."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Miten:
Tässä on, miten voit kirjoittaa ja käyttää funktioita Haskellissa:

```Haskell
-- Määritellään yksinkertainen funktio kahden numeron yhteenlaskuun
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- Funktion käyttö
main = print (addNumbers 3 5)
```

Tuloste:
```
8
```

Voit myös luoda korkeamman asteen funktioita:

```Haskell
-- Ottaa funktion ja soveltaa sitä kahdesti johonkin
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Käyttää applyTwice-funktiota anonyymin funktion kanssa
main = print (applyTwice (*2) 5)
```

Tuloste:
```
20
```

## Syventävä osio
Haskell, joka on puhtaasti funktionaalinen kieli, kohtelee funktioita ensiluokkaisen kansalaisen asemassa. Historiallisesti tämä juontaa juurensa lambda-kalkyylista, joka on laskennan perustava teoreettinen viitekehys. Toisin kuin imperatiivisissa kielissä, joissa funktiot ovat sarja ohjeita, Haskellissa funktiot ovat ilmaisuja, jotka kuvaavat suhteita tietojen välillä.

On vaihtoehtoja alastomien funktioiden kirjoittamiselle uudelleenkäyttöön. Harkitse tyyppiluokkien käyttöä polymorfismia varten tai moduulien hyödyntämistä liittyvien funktioiden ryhmittelyyn. Haskellin laiska evaluointi vaikuttaa myös funktion toteutukseen – funktiot eivät evaluoidu ennen kuin niiden tuloksia tarvitaan, mikä voi vaikuttaa suorituskykyyn liittyviin seikkoihin.

## Katso myös
- Virallinen Haskell-dokumentaatio: https://www.haskell.org/documentation/
- "Learn You a Haskell for Great Good!" kirjoittanut Miran Lipovača, aloittelijaystävällinen kirja: http://learnyouahaskell.com/
- "Real World Haskell" kirjoittaneet Bryan O'Sullivan, Don Stewart ja John Goerzen: http://book.realworldhaskell.org/
