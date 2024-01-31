---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testy pozwalają sprawdzić, czy nasz kod robi to, co powinien. Programiści piszą je, żeby zapobiegać błędom i ułatwić późniejsze zmiany w kodzie.

## How to:
Elm używa `elm-test` do pisania i uruchamiania testów. Oto przykład prostego testu:

```Elm
import Expect
import Test exposing (..)

suite : Test
suite =
  describe "Prosty test"
    [ test "Czy 2 + 2 to 4?" <| \_ ->
        2 + 2 |> Expect.equal 4
    ]

-- Uruchomienie testów
-- elm-test
```

Wynik testu:

```
TEST RUN PASSED

Prosty test
    Czy 2 + 2 to 4?: passed.
```

## Deep Dive
Elm wprowadził `elm-test` na początku swojej historii, uczynił ten pakiet standardem do testowania. Istnieją alternatywy jak `node-test-runner`, ale `elm-test` pozostaje najczęściej wybieranym rozwiązaniem. Testy w Elm są typowo pisane w deklaratywnym stylu, a `elm-test` umożliwia tworzenie zarówno prostych, jak i złożonych asercji oraz izolowanie efektów ubocznych.

## See Also
- Dokumentacja `elm-test`: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Przewodnik po Elm: https://guide.elm-lang.org/tests/
- Elm pakiet `Expect`: https://package.elm-lang.org/packages/elm-explorations/test/1.2.2/Expect
