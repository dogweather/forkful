---
title:                "Odczytywanie argumentów linii poleceń"
html_title:           "Elm: Odczytywanie argumentów linii poleceń"
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą szukającym efektywnych narzędzi do tworzenia interaktywnych aplikacji, to Elm z pewnością jest jednym z wyborów, który warto rozważyć. Poznajmy zatem, dlaczego warto poznać możliwości odczytywania argumentów wiersza poleceń przy użyciu tego języka.

## Jak to zrobić?

Odczytywanie argumentów wiersza poleceń w języku Elm jest bardzo proste. Wystarczy użyć funkcji `Elm.Platform.Args.flag`, która umożliwia dostęp do przekazanych argumentów jako listy wartości tekstowych.

Przykładowo, w przypadku podania argumentów ` --name John --age 30`, możemy odczytać je przy użyciu poniższego kodu:

```
Elm.Platform.Args.flag
    (Elm.List.fromList [ "name", "age" ])
    ( \name age -> Html.text ( "Witaj " ++ name ++ "! Masz " ++ age ++ " lat." ) )
```

Wynik powyższego przykładu będzie wyglądał następująco:

> Witaj John! Masz 30 lat.

## Głębszy zanurzenie

Możliwość odczytu argumentów wiersza poleceń może być szczególnie przydatna w przypadku tworzenia aplikacji internetowych z wykorzystaniem języka Elm. Możliwość przekazywania wartości, takich jak język użytkownika czy tryb pracy, pozwala na dostosowanie części aplikacji do ich preferencji.

Warto również wiedzieć, że `Elm.Platform.Args` udostępnia również funkcję `Elm.Platform.Args.map`, która pozwala na bardziej elastyczne mapowanie argumentów na inne typy wartości.

## Zobacz również

- [Dokumentacja Elm.Platform.Args](https://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Args)
- [Przykłady użycia argumentów wiersza poleceń w Elm](https://github.com/tizianoelemento/elm-flags)