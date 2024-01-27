---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere test consiste nel creare casi d'uso per provare che il codice funzioni come previsto. I programmatori li usano per assicurarsi che il codice sia robusto e per ridurre i bug durante gli aggiornamenti.

## Come fare:
```Elm
import Expect exposing (Expectation)
import Test exposing (Test, describe, test)
import Test.Runner.Node

suite : Test
suite =
    describe "Esempio test"
        [ test "Verifica dell'addizione" <|
            \_ -> Expect.equal (2 + 3) 5
        , test "Confronto liste" <|
            \_ -> Expect.equal [1, 2, 3] [1, 2, 3]
        ]

-- Per eseguire il test, usa il seguente comando nella CLI:
-- elm-test
```

Output previsto:
```
TEST RUN PASSED

Esempio test
    Verifica dell'addizione: PASSED
    Confronto liste: PASSED

2 tests ran, all passed
```

## Approfondimento
Elm rende i test una comodità piuttosto che un'opzione. A differenza di JavaScript, dove il testing si è evoluto con più framework, Elm ha `elm-test` come scelta principale, creato in linea con l'architettura e i tipi del linguaggio. Alternative in Elm sono limitate e inclinate verso specifici casi d'uso. Al cuore di `elm-test`, c'è la concezione che ogni test ritorna una `Test` che descrive l'esito atteso.

## Altre Risorse
- Documentazione di `elm-test`: [elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
