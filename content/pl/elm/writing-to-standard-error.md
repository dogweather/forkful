---
title:                "Elm: Pisanie do standardowego błędu"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego warto pisać do standardowego wyjścia błędów

Inżynierowie programiści często używają standardowego wyjścia błędów w celu wyświetlenia informacji o błędach i ostrzeżeń w trakcie działania programu. Jest to przydatne narzędzie do debugowania i poprawiania kodu. Oprócz tego, pisanie do standardowego wyjścia błędów jest również ważne ze względów bezpieczeństwa, ponieważ informuje użytkowników o potencjalnych problemach w aplikacji.

## Jak napisać do standardowego wyjścia błędów w Elm

W Elm, aby napisać do standardowego wyjścia błędów, możemy użyć funkcji `Debug.crash`. Przyjmuje ona argument typu `String` i zwraca wartość typu `Never`, co oznacza, że ta funkcja nigdy nie zwraca żadnego wyniku.

```Elm
import Debug exposing (crash)

main = 
    result = 5 / 0 -- dzielenie przez zero wygeneruje błąd
    Debug.crash "Nie można dzielić przez zero"
```

W powyższym przykładzie, gdy wywołana zostanie funkcja `crash`, wygenerowany zostanie błąd na standardowym wyjściu błędów z przekazanym przez nas komunikatem.

Możemy również wykorzystać funkcję `Debug.log` do wyświetlania informacji o błędach i ostrzeżeń. Ta funkcja przyjmuje dwa argumenty: `String` i wartość dowolnego typu, który chcemy wyświetlić. Zwraca ona ten sam typ wartości, co argument drugi.

```Elm
import Debug exposing (log)

main = 
    result = 5 / 0 -- dzielenie przez zero wygeneruje błąd
    Debug.log "Wynik dzielenia" result
```

## Głębszy wgląd w pisanie do standardowego wyjścia błędów

W Elm, standardowe wyjście błędów jest obsługiwane przez funkcję `Platform.sendToApp`. Jest to funkcja wbudowana w Elm i nie jest przeznaczona do używania poza platformą. Pozwala ona na wysłanie komunikatu do aplikacji, gdzie może być obsłużony przez programistę.

Ponadto, możemy również wykorzystać funkcję `Task.perform` do obsługi błędów i ostrzeżeń w naszej aplikacji Elm. Ta funkcja przyjmuje dwa argumenty: `Result error value` i funkcję do obsługi błędów (znajdująca się w drugim argumencie). W ten sposób możemy dokładnie kontrolować, co dzieje się z błędami w naszej aplikacji.

## Zobacz również
- [Elm - Debug Module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm - Using Debug.log](https://guide.elm-lang.org/debugging/debugging_concepts.html)
- [Elm - Error Handling with Task and Result](https://guide.elm-lang.org/error_handling/)