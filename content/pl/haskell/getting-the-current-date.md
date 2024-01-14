---
title:                "Haskell: Pobieranie aktualnej daty"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dzisiaj na tapecie mamy podstawy programowania w języku Haskell i dowiemy się, jak uzyskać aktualną datę w naszych programach. Ale dla niewtajemniczonych, pewnie zastanawiacie się dlaczego potrzebujemy aktualnej daty w naszym kodzie. Oto kilka powodów:

- Możemy użyć aktualnej daty w naszych aplikacjach do śledzenia czasu wykonania operacji.
- Może być przydatne w tworzeniu aplikacji bankowych lub innych związanych z finansami, gdzie ważne jest śledzenie daty transakcji.
- Aktualna data może nam również pomóc w rozwiązywaniu problemów związanych z czasem w naszym kodzie.

## Jak to zrobić

Zanim przejdziemy do głębszej analizy, zobaczmy najpierw przykładowy kod w Haskell, który pomoże nam uzyskać aktualną datę. Wystarczy użyć funkcji `getCurrentTime` z modułu `Data.Time`.

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    print now
```

Output:
```
2021-11-12 11:30:45.331621 UTC
```

Powyższy kod wyświetli nam aktualną datę i czas w formacie `RRRR-MM-DD GG:MM:SS.ssssss UTC`. Możemy również wykorzystać różne funkcje z modułu `Data.Time.Format`, aby formatować datę w odpowiedni dla nas sposób.

## Mocniejsze wiosło

Jeśli chcesz zgłębić temat i poznać więcej funkcji związanych z datami w Haskellu, polecam zapoznać się z dokumentacją pakietu `time` oraz `time-locale-compat`. Pakiet `time` zawiera moduły z funkcjami do zarządzania czasem, m.in. `Data.Time`, a `time-locale-compat` jest przydatny przy formatowaniu daty w różnych językach.

## Zobacz również

- Dokumentacja pakietu `time`: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
- Dokumentacja pakietu `time-locale-compat`: [https://hackage.haskell.org/package/time-locale-compat](https://hackage.haskell.org/package/time-locale-compat)

Teraz już wiesz, jak uzyskać aktualną datę w swoich programach w języku Haskell. Mam nadzieję, że ten post był dla Ciebie pomocny!