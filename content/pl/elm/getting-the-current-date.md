---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:14:15.974179-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i Dlaczego?)
Pobieranie aktualnej daty to jedno z zadań, które pozwala aplikacji rozumieć „teraz". Programiści robią to, aby śledzić czas, rejestrować zdarzenia lub wyświetlać odświeżone informacje użytkownikom.

## How to:
(Jak to zrobić:)
W Elm do pobierania aktualnej daty używamy pakietu `elm/time`. Oto jak to zrobić:

```Elm
import Time exposing (Posix)
import Task

-- Pozyskanie aktualnej daty i czsu jako Posix (milisekundy od epoki)
getCurrentDate : Task.Task Time.Error Posix
getCurrentDate = Time.now

-- Przykład użycia w aplikacji
type Msg = SaveTheDate Posix

saveCurrentDate : Cmd Msg
saveCurrentDate =
  Task.perform SaveTheDate getCurrentDate
```

Jeśli uruchomisz `saveCurrentDate`, otrzymasz komendę, która zapisze aktualny czas i datę i wyśle jako wiadomość `SaveTheDate`.

## Deep Dive
(Zanurzenie się głębiej)
Elm korzysta z typu Posix by reprezentować czas; to liczba milisekund od północy UTC, 1 stycznia 1970 r. Alternatywy? W językach frontendowych często używamy `Date` w JavaScript, ale Elm preferuje czysto funkcyjne podejście, stąd `Posix`. Szczegóły implementacyjne? `Time.now` wykonuje zadanie, a `Task.perform` przetwarza wynik do wiadomości w Twojej aplikacji.

## See Also
(Zobacz również)
- Oficjalna dokumentacja pakietu `elm/time`: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Guide na temat obsługi czasu: [https://guide.elm-lang.org/effects/time.html](https://guide.elm-lang.org/effects/time.html)
- Elm Discourse, jeśli masz pytania: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
