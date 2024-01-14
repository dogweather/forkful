---
title:                "Elm: Pobieranie aktualnej daty"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach programowanie stało się nieodłączną częścią naszego życia. Jest wiele języków programowania do wyboru, ale jeden z nich wyróżnia się swoją prostotą i skutecznością - jest nim Elm. W tym artykule dowiesz się, dlaczego warto poznać ten język, a w szczególności jak uzyskać aktualną datę.

## Jak to zrobić

Aby uzyskać aktualną datę w Elm, musimy użyć funkcji `Time.now`. Poniżej znajduje się przykładowy kod, który wyświetla aktualną datę i czas w konsoli:

```Elm
import Html exposing (text)
import Time exposing (now)

main =
  now
    |> Time.toString
    |> text
```

Wyjściem z powyższego kodu będzie coś w stylu: "Mar 8, 2021, 12:00:00 PM".

Możemy także użyć funkcji `Time.toIsoString` aby uzyskać datę i czas w formacie ISO:

```Elm
import Html exposing (text)
import Time exposing (now, toIsoString)

main =
  now
    |> Time.toIsoString
    |> text
```

Wyjście w tym przypadku będzie wyglądać tak: "2021-03-08T12:00:00Z".

## Wgląd w szczegóły

Funkcja `Time.now` zwraca wynik typu `Time.Posix` który reprezentuje datę i czas w formacie Unix. Możemy także użyć funkcji `Time.toYearMonthDay` i `Time.toHourMinuteSecond` aby uzyskać dokładne wartości rok, miesiąc, dzień, godzina, minuta i sekunda.

Możemy również manipulować czasem, wykorzystując funkcje takie jak `Time.add` lub `Time.subtract`, których możemy użyć aby dodać lub odjąć od aktualnego czasu określoną ilość sekund, minut, godzin itp.

Warto także wspomnieć o tym, że Elm jest językiem funkcyjnym, więc zawsze uzyskamy tę samą wartość dla odpowiedniego wejścia. To oznacza, że wywołując funkcję `Time.now` w różnych miejscach w naszym kodzie, uzyskamy tę samą aktualną datę i czas.

## Zobacz również

- Dokumentacja funkcji `Time.now` w Elm: https://package.elm-lang.org/packages/elm/time/latest/Time#now
- Oficjalna strona języka Elm: https://elm-lang.org/
- Pomocne artykuły i materiały o Elm: https://github.com/isRuslan/awesome-elm#articles-and-tutorials