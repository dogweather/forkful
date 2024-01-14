---
title:    "Elm: Wydruki z informacjami diagnostycznymi"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego

Wyświetlanie danych debugowania jest niezbędnym narzędziem dla każdego programisty Elm. Pozwala nam na lepsze zrozumienie działania naszego kodu, a także ułatwia nam znalezienie i naprawienie błędów. W tym artykule dowiesz się, dlaczego warto używać wyświetlacza danych debugowania w swoich projektach.

## Jak

Aby wyświetlić dane debugowania w Elm, możemy użyć funkcji `Debug.log`. Ta funkcja przyjmuje dwa argumenty: nazwę i wartość, którą chcemy wyświetlić. Przykładowy kod można znaleźć poniżej:

```Elm
import Debug exposing (log)

myFunction : Int -> Int
myFunction x =
  let
    result = x * 2
  in
    Debug.log "Wynik" result
```

Kiedy wywołamy tę funkcję na przykład z wartością 5, w konsoli deweloperskiej zobaczymy następujący tekst: `Wynik: 10`. Możemy również wyświetlać dowolne inne wartości, takie jak napisy, listy czy nawet struktury danych.

## Przyjrzenie się bliżej

Choć wyświetlanie danych debugowania jest bardzo przydatne, nie powinno być używane w kodzie produkcyjnym. Aby je wyłączyć, wystarczy po prostu usunąć wywołania funkcji `Debug.log`. Warto również pamiętać, że `Debug.log` jest funkcją czysto pomocniczą i nie ma wpływu na działanie naszego programu.

## Zobacz również

- [Dokumentacja Elm - Debugging](https://guide.elm-lang.org/debugging/)
- [Artykuł na temat debugowania w języku Elm](https://medium.com/@joelgrus/elm-vs-the-universe-or-a-framework-reconsidered-3a55a5f9b512)
- [Poradnik dotyczący wyłączania funkcji debugowania w kodzie produkcyjnym](https://medium.com/@y434n/elm-debug-to-the-rescue-a4a1b0c7ea69)