---
title:                "Haskell: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego

Kalkulacja daty w przyszłości lub przeszłości może być przydatna w wielu przypadkach, takich jak planowanie ważnych wydarzeń czy obliczanie dokładnego wieku. W tym wpisie przedstawię przystępny sposób na wykonanie tego zadania za pomocą języka Haskell.

# Jak to zrobić

Pierwszym krokiem jest zaimportowanie modułu "Data.Time" do naszego kodu. Następnie należy zdefiniować zmienną zawierającą aktualną datę używając funkcji "getCurrentTime", a także zmienne dzienne, miesięczne i roczne, które będą odpowiadać za przesunięcie w przeszłości lub przyszłości.

```Haskell
import Data.Time

currentDate :: IO UTCTime
currentDate = getCurrentTime

days :: (Num a) => a
days = 7

months :: (Num a) => a
months = 6

years :: (Num a) => a
years = 5
```

Następnie używając funkcji "addUTCTime" możemy obliczyć datę w przyszłości lub przeszłości. W poniższym przykładzie wyliczamy datę dwadzieścia dni w przyszłości.

```Haskell
futureDate :: IO UTCTime
futureDate = do
  current <- currentDate
  let future = addUTCTime (days * 20) current
  return future
```

Możemy również wyświetlić wynik jako czytelną dla użytkownika datę. W tym celu skorzystamy z funkcji "formatTime" i określimy odpowiednie formatowanie.

```Haskell
outputDate :: IO ()
outputDate = do
  date <- futureDate
  let formatted = formatTime defaultTimeLocale "%A, %d %B %Y" date
  putStrLn formatted
```

Przykładowe wywołanie "outputDate" wyświetli: "Monday, 29 March 2021".

# Pełne zanurzenie

Funkcje, które zostały użyte w powyższych przykładach opierają się na bibliotece "Data.Time", która oferuje wiele innych przydatnych narzędzi do pracy z datami. Warto zapoznać się z dokumentacją, aby poznać wszystkie dostępne możliwości.

# Zobacz również

- Dokumentacja do biblioteki Data.Time: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Wprowadzenie do Haskell: https://learnyouahaskell.com/chapters
- Przykłady formatowania dat i godzin w Haskell: https://wiki.haskell.org/Printing_a_locale-specific_time_and_date