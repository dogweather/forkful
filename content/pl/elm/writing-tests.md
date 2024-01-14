---
title:    "Elm: Pisanie testów"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego warto pisać testy w Elm

Pisanie testów jest ważnym elementem procesu tworzenia oprogramowania, gdyż pomaga w zapewnieniu jakości kodu oraz wykryciu błędów. W przypadku języka Elm testy są bardzo prostę w implementacji, co sprawia, że są idealne dla programistów, którzy chcą zadbać o jakość swojego kodu. Czytaj dalej, aby dowiedzieć się jak pisać testy w Elm oraz dlaczego warto to robić.

## Jak to zrobić

Aby zacząć pisać testy w Elm, wystarczy zainstalować bibliotekę `elm-test` za pomocą funkcji `elm install`. Następnie należy stworzyć plik z testami, który będzie zawierał funkcje testujące. Przykładowy kod wyglądać może następująco:

```Elm
module Testy exposing (..)

import Expect
import Foo exposing (..)

testy : Test
testy =
    describe "Testowanie funkcji add" [
        test "dodawanie liczb" <| \() ->
            Expect.equal (add 2 3) 5
        ,
        test "dodawanie ujemnych liczb" <| \() ->
            Expect.equal (add -2 -3) -5
        ,
        test "dodanie do zera" <| \() ->
            Expect.equal (add 5 0) 5
    ]
```

W powyższym przykładzie testujemy funkcję `add`, która dodaje dwie liczby. Każde testowanie zaczyna się od wywołania funkcji `describe`, która przyjmuje jako argument nazwę testowanej funkcji oraz listę testów. Każdy test z kolei składa się z nazwy oraz funkcji, która zawiera asercję wyrażającą oczekiwany rezultat wywołania testowanej funkcji.

## Pogłębiona analiza

Testy w Elm pozwalają nie tylko sprawdzić czy nasz kod działa poprawnie, ale również pomagają w zapewnieniu jego jakości. Dzięki temu, że piszemy testy od samego początku projektu, unikamy później dużych problemów wynikających z błędów w kodzie. Testy pomagają również w refaktoryzacji kodu, gdyż zmiany w funkcjonalności można szybko sprawdzić, uruchamiając tylko wybrane testy.

Funkcje testujące w Elm są też bardzo czytelne i intuicyjne, co ułatwia pracę zarówno początkującym, jak i doświadczonym programistom. Ponadto, Elm udostępnia narzędzia do pokazywania pokrycia kodu testami oraz automatycznego wykonywania testów po każdej zmianie w kodzie, co znacznie ułatwia proces pisania i utrzymywania testów.

## Zobacz także

- [Dokumentacja Elm: Testy](https://elm-lang.org/docs/testing)
- [Poradnik testowania w Elm](https://dev.to/rbasso/how-to-test-in-elm-dfe)
- [10 powodów dla których warto uczyć się Elm](https://www.youtube.com/watch?v=CleFk3BZBBI)