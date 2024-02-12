---
title:                "Koodin järjestäminen funktioihin"
aliases:
- fi/haskell/organizing-code-into-functions.md
date:                  2024-01-26T01:10:21.757800-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin Haskellissa tarkoittaa koodin pilkkomista uudelleenkäytettäviin, nimettyihin lohkoihin. Miksi? Se pitää koodisi DRY:nä (Don't Repeat Yourself, Älä toista itseäsi), tekee siitä luettavampaa ja helpommin debugattavaa.

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
