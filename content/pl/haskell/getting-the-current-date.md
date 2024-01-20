---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pobieranie aktualnej daty w Haskellu

## Co i Dlaczego?
Pobieranie aktualnej daty to powszechne zadanie, które polega na uzyskaniu bieżącego dnia, miesiąca i roku. Programiści robią to, aby móc śledzić czas wykonania operacji, rejestrowaniem danych czy prostym produkowaniem stempli czasowych.

## Jak to zrobić:

```Haskell
import Data.Time

main = do
    current <- getCurrentTime
    print current
```

Podczas uruchomienia tego kodu, uzyskamy wynik podobny do poniższego:

```Haskell
2021-04-29 11:15:41.703853 UTC
```

## Głębsze omówienie

**Kontekst historyczny**
Biblioteka `Data.Time` w Haskellu istnieje już od dłuższego czasu i jest standardowym rozwiązaniem do obsługi czasu w tym języku.

**Alternatywy**
Możliwe są również bardziej szczegółowe manipulacje z datą i czasem, używając innych bibliotek, takich jak `time-lens`, `time-parsers` itd.

**Szczegóły implementacji**
Możemy pobrać bardziej szczegółowe dane o czasie stosując różne funkcje, które zgłębiają typy danych z biblioteki `Data.Time`. Na przykład, `utctDay current` zwraca tylko datę bez czasu.

```Haskell
import Data.Time

main = do
    current <- getCurrentTime
    print $ utctDay current
```

Gdzie wynikiem powyższego kodu będzie:

```Haskell
2021-04-29
```

## Zobacz także

- [Dokumentacja Data.Time](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Projekt time-lens w GitHub](https://github.com/ekmett/lens)