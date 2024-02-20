---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:42.217793-07:00
description: "Een string converteren naar kleine letters betekent dat alle alfabetische\
  \ tekens worden omgezet naar hun kleine lettervorm. Programmeurs doen dit vaak\u2026"
lastmod: 2024-02-19 22:05:09.764089
model: gpt-4-0125-preview
summary: "Een string converteren naar kleine letters betekent dat alle alfabetische\
  \ tekens worden omgezet naar hun kleine lettervorm. Programmeurs doen dit vaak\u2026"
title: Een string omzetten naar kleine letters
---

{{< edit_this_page >}}

## Wat & Waarom?

Een string converteren naar kleine letters betekent dat alle alfabetische tekens worden omgezet naar hun kleine lettervorm. Programmeurs doen dit vaak voor hoofdletterongevoelige vergelijkingen of voor de normalisatie van tekstgegevens voor opslag en verwerking.

## Hoe te:

Elm gebruikt de `String.toLower` functie om tekst te converteren:

```elm
import String

lowercaseString : String -> String
lowercaseString text =
    String.toLower text

-- Gebruik
result : String
result =
    lowercaseString "HeLLo, WoRLD!"

-- Uitvoer: "hello, world!"
```

## Diepere Duik

Elm's `String.toLower` komt uit Elm's kern `String` bibliotheek, met internationalisering in gedachten. Historisch gezien is de conversie van hoofdletters geëvolueerd van basis ASCII naar volledige Unicode-ondersteuning vanwege de behoefte aan internationale tekstverwerking.

In sommige talen, zoals JavaScript, zijn er alternatieven zoals `toLowerCase()` en `toLocaleLowerCase()`, waarbij de laatste rekening houdt met locatiespecifieke regels. In Elm zou `String.toLower` in de meeste gevallen moeten volstaan, tenzij men te maken heeft met locatiegevoelige bewerkingen, die mogelijk een aangepaste implementatie vereisen.

Een detail om te onthouden is dat de omzetting van hoofdletters niet altijd één-op-één is; sommige tekens hebben mogelijk geen kleine letter equivalent, en andere kunnen van grootte veranderen (bijv., het converteren van "ß" in het Duits).

## Zie Ook

- Elm String documentatie: [https://package.elm-lang.org/packages/elm/core/latest/String#toLower](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- Unicode Case Folding: [https://www.w3.org/International/wiki/Case_folding](https://www.w3.org/International/wiki/Case_folding)
- Taalspecifieke kwesties met hoofdletteromzetting: [https://stackoverflow.com/questions/234591/upper-vs-lower-case](https://stackoverflow.com/questions/234591/upper-vs-lower-case)
