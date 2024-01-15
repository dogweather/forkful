---
title:                "Pobieranie aktualnej daty"
html_title:           "Haskell: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Obecnie wiele aplikacji wymaga wyświetlania aktualnej daty, aby użytkownicy mogli śledzić ostatnie zmiany lub planować swoje zadania. W Haskellu, języku programowania o silnym typowaniu i wielu zaletach, istnieje wiele sposobów na uzyskanie aktualnej daty. W tym artykule dowiesz się, jak można to zrobić w prosty i efektywny sposób.

## Jak to zrobić

Istnieje kilka bibliotek w Haskellu, które umożliwiają uzyskanie aktualnej daty. Jedną z nich jest biblioteka 'time', która jest częścią standardowej biblioteki języka. Aby użyć tej biblioteki, wystarczy dodać następującą linię kodu na początku pliku:

``` Haskell
import Data.Time
```

Następnie możesz wykorzystać funkcję 'getCurrentTime' z tej biblioteki, aby uzyskać aktualny czas wraz z datą. Oto przykładowy kod:

``` Haskell
main = do
    czas <- getCurrentTime
    print czas
```

Wywołanie tej funkcji zwróci wartość typu 'UTCTime', która reprezentuje ustalony punkt w czasie. Aby uzyskać tylko datę, możesz użyć funkcji 'localDay' z biblioteki 'Data.Time.Calendar', która zwróci datę w postaci 'Day'. Oto przykładowy kod:

``` Haskell
import Data.Time
import Data.Time.Calendar

main = do
    czas <- getCurrentTime
    let data = localDay (utcToLocalTime (TimeZone (60 * 60) False "CET") czas)
    print data
```

Zauważ, że użyliśmy też funkcji 'utcToLocalTime', aby dopasować czas do lokalnej strefy czasowej. W powyższym przykładzie ustawiliśmy strefę na Centralnoeuropejski Czas Letni (CET).

## Zagłębienie

Jeśli interesuje Cię więcej informacji na temat biblioteki 'time' i innych sposobów na uzyskanie aktualnej daty w Haskellu, warto zapoznać się z oficjalną dokumentacją: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html.

Możesz również eksperymentować z różnymi funkcjami i typami danych związanych z datami, takimi jak 'ZonedTime' czy 'NominalDiffTime'.

## Zobacz także

- Dokumentacja biblioteki 'time': https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Przykładowy projekt wykorzystujący bibliotekę 'time': https://github.com/haskell-servant/servant/blob/master/examples/no-csrf.hs
- Inne sposoby na uzyskanie aktualnej daty w Haskellu: https://wiki.haskell.org/Date_and_time_libraries