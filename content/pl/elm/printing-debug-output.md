---
title:                "Drukowanie komunikatów debugowania"
html_title:           "Haskell: Drukowanie komunikatów debugowania"
simple_title:         "Drukowanie komunikatów debugowania"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Drukowanie debugowania to metoda, za pomocą której programiści mogą widzieć, co się dzieje w ich kodzie na bieżąco. Jest to nieoceniony składnik przepływu pracy, który pomaga szybko znaleźć i naprawić błędy.

## Jak to zrobić:
Elm wyposażony jest w funkcję `Debug.log`, która jest świetnym narzędziem do debugowania. Przykład:

```Elm
let
    _ = Debug.log "Value of x" x
in
(...)
```

W powyższym przykładzie, gdy program dojdzie do tego punktu, wydrukuje "Value of x" wraz z aktualną wartością `x`.

## Na głęboką wodę:
1. Kontekst historyczny: Elm, pomimo swojej relativej młodości (pierwsze wydanie w 2012 roku), stara się ułatwić proces debugowania to formatu designu języka.

2. Alternatywy: Istnieją inne narzędzia do debugowania, jak `Debug.toString` lub komercyjne narzędzia od innych dostawców, ale `Debug.log` to najprostsze i najbardziej dostępne rozwiązanie.

3. Szczegóły implementacji: Co ważne, `Debug.log` zwraca przekazane do niej wartości bez zmian, co sprawia, że jego umieszczanie w dowolnym miejscu kodu jest nieskomplikowane.

## Zobacz też:
- Elm Guide do Debugowania: https://guide.elm-lang.org/effects/log.html
- Elm Debug API: https://package.elm-lang.org/packages/elm/core/latest/Debug