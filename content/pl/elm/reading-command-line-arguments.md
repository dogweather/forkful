---
title:    "Elm: Wczytywanie argumentów wiersza poleceń"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedyś, jak można przekazać zmienne do swojego programu w Elm z linii poleceń? Może chcesz, aby użytkownik mógł wprowadzić własne ustawienia lub opcje? W tym blogu dowiesz się, jak to zrobić!

## Jak to zrobić

Aby czytać argumenty z linii poleceń w Elm, musimy użyć wbudowanej biblioteki `Platform`. Najpierw musimy zaimportować tę bibliotekę:

```Elm
import Platform exposing (..)
```

Następnie użyjemy funkcji `getArgs()` aby uzyskać listę argumentów:

```Elm
getArgs() 
```

Jeśli chcesz, aby użytkownik mógł przekazać zmienne z opcjami, możesz użyć funkcji`getOptions()`, która zwróci listę par klucz-wartość:

```Elm
getOptions()
```

Teraz możemy przetworzyć te argumenty i opcje używając funkcji `List.map` lub `Dict.get` w zależności od tego, jak chcemy je wykorzystać w naszym programie.

## Deep Dive

Bardziej zaawansowaną techniką jest parsowanie argumentów z użyciem biblioteki `Elm Parser` lub `elm-arg-parser`. Pozwala to na dokładniejsze określenie struktury argumentów i tworzenie bardziej elastycznego kodu. Możliwości są praktycznie nieograniczone, więc warto zdecydować się na ten krok, gdy masz już pewną wiedzę na temat Elm i jego bibliotek.

## Zobacz także

- [Dokumentacja Platformy](https://package.elm-lang.org/packages/elm/core/latest/Platform)
- [Biblioteka Parsera ELm](https://package.elm-lang.org/packages/elm/parser/latest/)
- [Biblioteka Elm Arg Parser](https://package.elm-lang.org/packages/mpizenberg/elm-argv/latest/)