---
title:                "Elm: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Posiadanie aktualnej daty jest ważne w wielu aplikacjach, ponieważ pozwala nam śledzić czas i datę wykonania różnych operacji. W Elm istnieje wiele sposobów na uzyskanie bieżącej daty, a w tym artykule wyjaśnimy kilka z nich.

## Jak to zrobić

Istnieją dwie główne metody pobierania bieżącej daty w Elm - za pomocą wbudowanego modułu `Time` lub za pomocą zewnętrznej biblioteki `elm-time`.

### Metoda 1: Moduł Time

Pierwszą metodą jest użycie wbudowanego modułu `Time`, który zapewnia funkcję `now`. Przykładowy kod wyglądałby następująco:

```Elm
import Time exposing (now)

currDate = now
```

Po uruchomieniu tej funkcji otrzymamy wynik w postaci `Result` zawierający aktualną datę i czas. Musimy pamiętać, że czas jest reprezentowany jako wartość całkowita w milisekundach, więc musimy przekonwertować to na bardziej czytelną formę.

```Elm
convertDate result = case result of
    Ok time -> Date.fromTime time
    Err _ -> Date.fromTime 0

currDate = now
convertedDate = convertDate currDate
```

W powyższym kodzie, najpierw definiujemy funkcję, która przekonwertuje wynik na datę czytelną przez Elm. Następnie wywołujemy funkcję `now` i przekazujemy jej wynik do funkcji `convertDate`.

### Metoda 2: Biblioteka Elm-time

Drugą metodą jest użycie zewnętrznej biblioteki `elm-time`, która dostarcza bardziej zaawansowane funkcje do manipulowania czasem i datą. Możemy zainstalować tę bibliotekę za pomocą komendy `elm install justinmimbs/time`.

```Elm
import Time exposing (utc, Date)
import Time.Date exposing (Day, month, year, toYear, toMonth, toDay)
import Time.Extra exposing (localHour)

myTime = utc
myDate = Date.fromTime myTime

currYear = toYear myDate
currMonth = toMonth myDate
currDay = toDay myDate
currHour = localHour 2 myTime
```

W powyższym kodzie, używamy różnych funkcji dostępnych w bibliotece `elm-time` w celu pobrania poszczególnych składowych daty i czasu. Przykładowo, przy użyciu funkcji `localHour` możemy równolegle określić strefę czasową.

## Głębszy zanurzenie

Funkcje `now` i `utc` używają czasu komputera jako podstawy dla obliczeń daty i czasu. Możemy także użyć funkcji `since` i `sinceUtc` do ustalenia daty i czasu względem innej daty, na przykład daty początkowej naszego programu. Dzięki temu możemy śledzić odległość czasową od wybranego punktu.

## Zobacz także

- Dokumentacja modułu `Time` w [oficjalnej dokumentacji Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- Dokumentacja biblioteki `elm-time` w [oficjalnym repozytorium GitHub](https://github.com/justinmimbs/time)