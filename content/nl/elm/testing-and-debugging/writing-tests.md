---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:53.828008-07:00
description: "Tests schrijven betekent het cre\xEBren van code die controleert of\
  \ je hoofdcode werkt zoals verwacht. Programmeurs testen om bugs vroeg op te sporen,\u2026"
lastmod: '2024-03-13T22:44:50.728476-06:00'
model: gpt-4-0125-preview
summary: "Tests schrijven betekent het cre\xEBren van code die controleert of je hoofdcode\
  \ werkt zoals verwacht."
title: Tests Schrijven
weight: 36
---

## Hoe te:
Elm gebruikt `elm-test` voor het schrijven van tests. Hier is een kleine test voor een functie `add` die twee getallen optelt:

```Elm
importeer Verwacht
importeer Test met de expositie (..)
importeer AddingModule met de expositie (add)

suite : Test
suite =
    beschrijf "AddingModule"
        [ test "add functie test" <|
            \_ -> Verwacht.gelijk (add 1 2) 3
        ]

-- Om tests uit te voeren, gebruik je de volgende opdracht:
-- elm-test
```

Als `add` correct werkt, zal de output zijn:

```
TESTRUN GESLAAGD

Duur: 42 ms
Geslaagd:   1
Mislukt:   0
```

## Diepere Duik
Elm's testframework, `elm-test`, biedt een snelle, betrouwbare manier om unittests te schrijven. Het moedigt TDD (Test-Driven Development) aan. Voor `elm-test` bestonden alternatieven zoals `elm-check`, maar die waren niet zo geïntegreerd. Wat implementatie betreft, gebruikt `elm-test` pure functies vrij van bijeffecten, wat perfect aansluit bij Elm's architectuur.

## Zie Ook
- Elm's officiële testdocumentatie: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Een artikel over Elm's testpatronen: https://elmprogramming.com/testing.html
- Het `elm-test` pakket op GitHub: https://github.com/elm-explorations/test
