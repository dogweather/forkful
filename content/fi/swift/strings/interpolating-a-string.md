---
date: 2024-01-20 17:51:53.236357-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:56.893726-06:00'
model: gpt-4-1106-preview
summary: .
title: Merkkijonon interpolointi
weight: 8
---

## How to:
```Swift
let name = "Matti"
let age = 30
let greeting = "Hei, nimeni on \(name) ja olen \(age) vuotta vanha."
print(greeting)
```
Output:
```
Hei, nimeni on Matti ja olen 30 vuotta vanha.
```

## Deep Dive
Merkkijonon interpolointi otettiin Swiftiin käyttöön heti kielen ensiversioissa, helpottamaan merkkijonojen muodostamista ilman että tarvittaisiin monimutkaisia konkatenointeja kuten vaikkapa Objective-C:ssä. Aiempina aikoina päätyttiin käyttämään sprintf-tyylistä formaattia tai jäykkiä merkkijonoja, jotka eivät sopineet dynaamisiin sisältöihin. Swiftissä interpolointi tapahtuu lisäämällä arvo suoraan merkkijonoon käyttäen \(arvo) syntaksia.

Vaihtoehtoina merkkijonon interpolointiin voisi käyttää konkatenointia tai String(format:)-metodia, mutta nämä ovat usein sekavampia ja virhealttiimpia tapoja. Tarkemmalla tasolla, kun interpoloit merkkijonoa Swiftissä, kompilaattori korvaa interpoloidut osat niiden merkkijonoesityksillä runtime-aikana. Tämä mahdollistaa monimutkaisten lausekkeiden laskemisen ja muuttujien arvojen upottamisen suoraan merkkijonoon ilman ylimääräistä vaivaa.

## See Also
- Swiftin virallinen dokumentaatio merkkijonojen interpoloinnista: [Swift String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- Swiftin kehittäjäfoorumi: [Swift Forums](https://forums.swift.org)
