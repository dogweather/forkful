---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:38.912985-07:00
description: "Pisanie test\xF3w w Elm polega na tworzeniu przypadk\xF3w testowych\
  \ w celu zweryfikowania poprawno\u015Bci kodu Elm, zapewniaj\u0105c, \u017Ce dzia\u0142\
  a on zgodnie z\u2026"
lastmod: 2024-02-19 22:04:54.455455
model: gpt-4-0125-preview
summary: "Pisanie test\xF3w w Elm polega na tworzeniu przypadk\xF3w testowych w celu\
  \ zweryfikowania poprawno\u015Bci kodu Elm, zapewniaj\u0105c, \u017Ce dzia\u0142\
  a on zgodnie z\u2026"
title: "Pisanie test\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w Elm polega na tworzeniu przypadków testowych w celu zweryfikowania poprawności kodu Elm, zapewniając, że działa on zgodnie z oczekiwaniami. Programiści robią to, aby wyłapywać błędy wcześnie, ułatwiać utrzymanie i poprawiać jakość oraz niezawodność swoich aplikacji.

## Jak to zrobić:

Elm używa pakietu `elm-explorations/test` do pisania testów jednostkowych i testów fuzz. Zacznij od dodania pakietu do swojego projektu:

```elm
elm install elm-explorations/test
```

Utwórz plik testowy, na przykład `tests/ExampleTest.elm`, i zaimportuj moduły testujące. Oto prosty test, który weryfikuje funkcję `add : Int -> Int -> Int`:

```elm
module ExampleTest exposing (..)

import Expect
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Prosta funkcja dodawania"
        [ test "Dodawanie 2 i 3 daje 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Aby uruchomić swoje testy, będziesz potrzebować `elm-test`:

```shell
npm install -g elm-test
elm-test
```

To skompiluje twoje testy i wydrukuje wyniki w terminalu. Dla powyższego przykładu, wyjście powinno wyglądać mniej więcej tak:

```
TEST RUN PASSED

Duration: 42 ms
Passed:   1
Failed:   0
```

Dla bardziej złożonego przykładu, powiedzmy, że chcesz przetestować funkcję `add` testem fuzz, aby upewnić się, że poprawnie obsługuje ona szeroki zakres wejść całkowitoliczbowych. W takim przypadku zmodyfikuj swój plik `ExampleTest.elm` w następujący sposób:

```elm
module ExampleTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (..)
import YourModuleName exposing (add)

suite : Test
suite =
    describe "Testowanie funkcji add z użyciem fuzzingu"
        [ fuzz int "Fuzz testing dodawania z losowymi intami" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Uruchom ponownie `elm-test`, aby zobaczyć testy fuzz w akcji. Wyjście będzie zmieniać się w zależności od losowego wejścia, ale udane testy wskażą brak awarii:

```
TEST RUN PASSED

Duration: 183 ms
Passed:   100
Failed:   0
```

Te przykłady pokazują, jak pisać i uruchamiać proste testy jednostkowe oraz testy fuzz w Elm, korzystając z pakietu `elm-explorations/test`. Testowanie jest kluczowym elementem procesu rozwoju, pomagającym zapewnić niezawodność aplikacji Elm i utrzymać wysoką jakość.
