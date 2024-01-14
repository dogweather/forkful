---
title:                "Elm: Wydrukowanie wyjścia debugowania"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Możliwość drukowania danych debugowania jest nieodzownym narzędziem dla programistów Elm. Dzięki niej możemy łatwo śledzić przebieg naszego kodu i znaleźć ewentualne błędy.

## Jak używać

Moduł Debug w Elm zawiera funkcje do drukowania danych debugowania, takich jak `toString` czy `toFloat`. Możemy je wykorzystać w naszym kodzie, umieszczając je wewnątrz `Debug.log` w następujący sposób:

```Elm
Debug.log "Zmienna x" (toString x) 
```

W ten sposób możemy wydrukować wartość zmiennej `x` w konsoli przeglądarki.

## Głębsze zanurzenie

Funkcja `Debug.log` jest nie tylko przydatna do wyświetlania wartości zmiennych, ale także do śledzenia przebiegu działania naszego programu. Możemy wykorzystać ją do drukowania kolejnych kroków w naszym kodzie, dzięki czemu łatwiej będzie nam znaleźć ewentualne błędy.

## Zobacz także

- [Dokumentacja Elm Debug](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Przykłady użycia Debug w Elm](https://elmprogramming.com/debugging-in-elm-using-elm-debug-log.html)