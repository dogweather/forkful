---
title:                "Haskell: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego warto poznać bieżącą datę? W dzisiejszym wpisie na temat programowania w Haskellu przedstawimy Ci, jak w prosty sposób uzyskać aktualną datę w swoich programach. Poznasz przydatne funkcje i narzędzia, które przydadzą się w tworzeniu praktycznych aplikacji.

## Jak to zrobić

Wykorzystując język Haskell i bibliotekę "time", odczytanie bieżącej daty jest bardzo proste. Wystarczy użyć funkcji "getCurrentTime", która zwraca wartość typu "UTCTime". Oto przykładowy kod:

```Haskell
import Data.Time

main = do
  czas <- getCurrentTime
  print czas
```

Powyższy kod wyświetli aktualną datę i czas w formacie "YYYY-MM-DD hh:mm:ss". Jeśli chcesz wyświetlić tylko datę, możesz skorzystać z funkcji "utctDay" i przekazać jako argument otrzymaną wartość "UTCTime". Oto przykład:

```Haskell
import Data.Time

main = do
  czas <- getCurrentTime
  let data = utctDay czas
  print data
```

W tak prosty sposób możesz odczytać aktualną datę i wykorzystać ją do swoich potrzeb. Ale co jeśli chcesz wyświetlić datę w innym formacie? Wystarczy skorzystać z funkcji "formatTime", której pierwszym argumentem jest format daty, a drugim otrzymana wartość "UTCTime". Oto przykładowy kod, który wyświetli datę w formacie "DD-MM-YYYY":

```Haskell
import Data.Time
import Data.Time.Format

main = do
  czas <- getCurrentTime
  let data = formatTime defaultTimeLocale "%d-%m-%Y" czas
  print data
```

## Głębszy wgląd

Zastanawiałeś się, jak dokładnie funkcja "getCurrentTime" odczytuje aktualną datę? W rzeczywistości, korzysta ona z systemowego zegara systemowego, a dokładniej z funkcji "gettimeofday" w systemach Unix i "GetSystemTime" w systemach Windows. Dzięki temu, data jest precyzyjna i zgodna z ustawieniami czasu na Twoim komputerze.

## Zobacz także

Jeśli chcesz pogłębić swoją wiedzę na temat zarządzania czasem w Haskellu, polecamy Ci zapoznać się z dokumentacją biblioteki "time" oraz z innymi przydatnymi funkcjami, takimi jak "addUTCTime" czy "diffUTCTime". Możesz również poszukać ciekawych zadań do wykonania związanych z datami i czasem w języku Haskell. Poniżej znajdziesz kilka linków, które mogą Ci się przydać:

- [Dokumentacja biblioteki "time"](https://hackage.haskell.org/package/time)
- [Przykładowe zadania związane z czasem w Haskellu](https://wiki.haskell.org/99_questions/Solutions/71_to_80)