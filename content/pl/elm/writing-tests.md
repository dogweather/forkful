---
title:                "Elm: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy do kodu w Elm

Testy są niezbędnym elementem w procesie tworzenia oprogramowania. Pozwalają one zweryfikować poprawność działania naszego kodu i uniknąć błędów w przyszłości. W Elm, pisanie testów jest łatwe i przyjemne, dzięki czemu możemy mieć pewność, że nasza aplikacja działa zgodnie z oczekiwaniami.

## Jak pisać testy w Elm

Pisanie testów w Elm opiera się na wykorzystaniu biblioteki "elm-test". W pierwszej kolejności musimy zainstalować ją w naszym projekcie przy użyciu komendy `elm install elm-test`. Następnie w pliku testowym możemy zacząć definiować testy za pomocą funkcji `test`, podając opis testu oraz funkcję, która przeprowadzi próbę. Poniżej znajduje się przykładowy kod testu:

```elm
module Tests exposing (..)

import Expect
import ElmTest

test : ElmTest.Test
test =
    ElmTest.test "2 + 2 jest równe 4" <| 
        \() -> 
            Expect.equal 4 (2 + 2)

```

W powyższym przykładzie testujemy, czy wynik dodawania dwóch liczb jest równy 4. Po zdefiniowaniu testu, możemy go uruchomić przy użyciu komendy `elm-test` w terminalu. Jeśli wszystkie testy zostaną zaliczone, ujrzymy informację o sukcesie, w przeciwnym wypadku zostaną wyświetlone błędy.

## Głębsze zagadnienia

Pisanie testów w Elm może być również pomocne przy projektowaniu aplikacji. Dzięki zastosowaniu techniki "TDD" (Test-Driven Development) możemy definiować testy przed napisaniem właściwego kodu, co pozwala na lepsze zrozumienie potrzeb naszej aplikacji. Dodatkowo, testy pomagają nam wychwycić błędy zanim trafią one do użytkowników.

## Zobacz również

- Dokumentacja biblioteki "elm-test": https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Wprowadzenie do pisanie testów w Elm: https://guide.elm-lang.org/testing/
- Poradnik "TDD" w Elm: https://thoughtbot.com/blog/elm-and-test-driven-development