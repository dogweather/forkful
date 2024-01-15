---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Elm: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie aktualnej daty jest niezbędne w wielu aplikacjach internetowych i programach. Dzięki temu możemy wyświetlić aktualną datę na stronie, kontrolować wygaśnięcie ważności dokumentów lub prowadzić statystyki na podstawie daty.

## Jak to zrobić

```elm
currentDate : Date
currentDate =
    Time.now
```

Powyższy kod używa wbudowanej funkcji `now` z modułu `Time`, która zwraca bieżącą datę i czas w formacie `Date`. Możemy wykorzystać tę datę do dalszych przetwarzania lub wyświetlenia jej na stronie.

Aby wyświetlić bieżącą datę w formacie tekstowym, możemy użyć funkcji `toText` z modułu `Time.Format`:

```elm
import Time.Format exposing (toText)

currentDate : String
currentDate =
    currentDate
        |> toText "dd/MM/yyyy"
```

Powyższy kod zwróci datę w formacie `dd/MM/yyyy`, czyli np. `08/03/2021`.

## Deep Dive

W Elm istnieje wiele różnych metod do pobierania, przetwarzania i wyświetlania bieżącej daty. Istnieje również możliwość tworzenia niestandardowych formatów daty przy użyciu modułu `Time.Format`. Wraz z rozwojem języka, może pojawić się coraz więcej sposobów na zarządzanie datami.

## Zobacz także

- Dokumentacja modułu `Time` w języku polskim: https://package.elm-lang.org/packages/elm/time/latest/Time
- Oficjalna strona języka Elm w języku polskim: https://guide.elm-lang.org/
- Przykładowe projekty w języku Elm: https://elm-projects.com/