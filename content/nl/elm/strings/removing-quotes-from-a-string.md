---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:04.637209-07:00
description: "Hoe: In Elm kun je de `String` functies gebruiken om strings te manipuleren,\
  \ zoals het verwijderen van aanhalingstekens. Hier is een eenvoudige manier om\u2026"
lastmod: '2024-03-13T22:44:50.712694-06:00'
model: gpt-4-0125-preview
summary: In Elm kun je de `String` functies gebruiken om strings te manipuleren, zoals
  het verwijderen van aanhalingstekens.
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe:
In Elm kun je de `String` functies gebruiken om strings te manipuleren, zoals het verwijderen van aanhalingstekens. Hier is een eenvoudige manier om dit te doen:

```Elm
removeQuotes : String -> String
removeQuotes str =
    String.trim (String.filter (\char -> char /= '\"' && char /= '\'') str)

main =
    String.removeQuotes "\"Dit is een 'gequote' string!\""
    -- Output: Dit is een gequote string!
```

Onthoud gewoon: dit kleine fragment verwijdert alle aanhalingstekens uit je string, dus gebruik het verstandig!

## Diepgaande Duik
In het verleden was het werken met strings een beetje meer hands-on, waarbij veel handmatig parsen betrokken was. Tegenwoordig maken talen zoals Elm het eenvoudiger met ingebouwde functies. De functie `String.filter` is een veelzijdig hulpmiddel in je arsenaal voor wanneer je elk karakter wilt onderzoeken, wat onder meer het verwijderen van aanhalingstekens omvat.

Als alternatief zou je kunnen overwegen om reguliere expressies te gebruiken als Elm deze op een draagbare manier zou ondersteunen, wat standaard niet het geval is. Maar hé, Elm's focus op eenvoud en veiligheid betekent dat onze aanpak met `String.filter` duidelijk, veilig en gemakkelijk te onderhouden is.

Elms functionele aanpak moedigt pure functies aan zonder bijwerkingen, en `removeQuotes` is een uitstekend voorbeeld. Het neemt een string en geeft een nieuwe terug, waarbij het origineel ongedeerd blijft. Dat zijn de onveranderlijke datastructuren van Elm in actie, die voorspelbaarheid bevorderen en je debugleed verlichten.

## Zie Ook
Voor verder lezen en aanverwante avonturen in stringmanipulatie, bekijk de `String` module documentatie van Elm op:

- [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String)

En als je ooit in de knel zit over wat Elm ondersteunt op het gebied van stringverwerking of enige taalfunctie:

- [Elm Language Guide](https://guide.elm-lang.org/)
