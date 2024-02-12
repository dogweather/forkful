---
title:                "Tests Schrijven"
aliases: - /nl/elm/writing-tests.md
date:                  2024-01-28T22:12:53.828008-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tests schrijven betekent het creëren van code die controleert of je hoofdcode werkt zoals verwacht. Programmeurs testen om bugs vroeg op te sporen, functionaliteit te garanderen en toekomstige aanpassingen minder riskant te maken.

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
