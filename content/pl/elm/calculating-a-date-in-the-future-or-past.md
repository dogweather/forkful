---
title:                "Elm: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości jest niezbędnym narzędziem w wielu aplikacjach internetowych. Dzięki temu można zaplanować przyszłe wydarzenia lub przewidzieć ważne daty w przeszłości. W tym artykule dowiesz się, jak w łatwy sposób obliczać daty w Elm.

## Jak to zrobić?

Aby obliczyć datę w Elm, musisz użyć wbudowanego modułu "Time". Przykładowy kod może wyglądać tak:

```Elm
import Time exposing (..)

futureDate = add 5 Days (millisToPosix 0)
pastDate = add -2 Months (millisToPosix 0)

main = text ("Data w przyszłości: " ++ toString futureDate ++ 
"\nData w przeszłości: " ++ toString pastDate)
```

W powyższym przykładzie, używając funkcji "add", można dodać lub odjąć określoną jednostkę czasu (np. dni, miesiące) od aktualnej daty, określonej przez funkcję "millisToPosix". Następnie za pomocą funkcji "toString" można przekonwertować wynik na czytelną dla człowieka formę. Takie obliczenia daty mogą być wykorzystane w wielu interesujących aplikacjach, na przykład do wyświetlania przyszłych urodzin użytkownika lub okresu, który minął od ważnego wydarzenia.

## Wgląd w obliczanie daty w przyszłości lub przeszłości

W celu głębszego zrozumienia, jak działają funkcje "add" i "millisToPosix", warto przeanalizować strukturę danych zawartą w modułach "Time" i "Posix". W module "Time" znajdują się funkcje do manipulacji czasem (np. "add", "subtract"), w tym funkcja "millisToPosix", która konwertuje wartość w milisekundach na typ danych "Posix". Moduł "Posix" zawiera funkcje do konwersji danych czasowych na odpowiednie formaty (np. "toString", "utc").

## Zobacz także

- Dokumentacja modułu "Time" w Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Dokumentacja modułu "Posix" w Elm: https://package.elm-lang.org/packages/elm/core/latest/Posix
- Przykładowe aplikacje Elm wykorzystujące obliczanie daty: https://github.com/himdel/elm-dateexamples