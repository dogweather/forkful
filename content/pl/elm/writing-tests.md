---
title:    "Elm: Pisanie testów"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne?

Pisanie testów jest kluczowym elementem w procesie tworzenia aplikacji w Elm. Poprzez zapewnienie pokrycia testami, możemy mieć pewność, że nasz kod działa zgodnie z oczekiwaniami w każdej sytuacji. Dzięki temu unikamy błędów w trakcie działania aplikacji i zapewniając jej stabilność.

## Jak pisać testy w Elm?

Aby rozpocząć pisanie testów w Elm, musimy najpierw zainstalować narzędzie elm-test. Następnie rozbudujemy naszą aplikację przy użyciu modułu Test i będziemy mogli pisać testy, które będą automatycznie uruchamiane wraz z naszą aplikacją. Poniższy przykład pokazuje, jak napisać test dla funkcji dodającej:

```Elm
add : Int -> Int -> Int
add x y =
    x + y

-- Test
import Test exposing (..)
import Expect exposing (expect)
import Main exposing (add)

tests : Test
tests =
    describe "Dodawanie" [
        test "2 + 3" (
            expect (add 2 3)
                |> toEqual 5
        ),
        test "5 + 5" (
            expect (add 5 5)
                |> toEqual 10
        )
    ]

main =
    run tests
```

Po uruchomieniu testów, otrzymamy poniższy wynik:

```
Dodawanie
  ✓ 2 + 3
  ✓ 5 + 5
```

Widzimy, że oba nasze testy przechodzą pomyślnie, co oznacza, że funkcja dodająca działa zgodnie z naszymi oczekiwaniami. 

## Głębsza analiza pisania testów w Elm

Pisanie testów w Elm jest bardzo łatwe dzięki narzędziu elm-test, które automatycznie uruchamia wszystkie nasze testy wraz z aplikacją. Dzięki temu mamy szybki i pewny sposób sprawdzania, czy nasz kod działa zgodnie z oczekiwaniami. Dodatkowo, pisząc testy, zmuszamy się do myślenia o różnych przypadkach działania naszego kodu, co przekłada się na lepszą jakość i niezawodność aplikacji.

## Zobacz także

- [Dokumentacja elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Poradnik dla początkujących w Elm](https://guide.elm-lang.org/)
- [Przykładowe aplikacje w Elm](https://github.com/elm/projects)