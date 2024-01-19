---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Parsowanie daty ze stringa polega na przekształceniu ciągu znaków reprezentującego datę w faktyczny obiekt daty w programie. Programiści robią to, by móc manipulować danymi daty i stosować różne operacje, takie jak porównywanie, sortowanie itp.

## Jak to zrobić:

W aktualnej wersji Gleam, biblioteka do obsługi dat i czasu nie jest jeszcze dostępna. Możesz jednak użyć funkcji Erlang'a do parsowania daty za pomocą interopcji. Oto jak:

```Gleam
import gleam/erlang/time.{StringToDate}

// Parsowanie daty ze stringa formatu "YYYY-MM-DD"
let date_string = "2021-12-01"
let maybe_date = StringToDate.string_to_date(date_string)
```

Kod zwróci datę w formacie `{Year, Month, Day}` jeśli string jest prawidłowy, w przeciwnym razie zwróci błąd.

## Dogłębne Zagłębianie:

Parsowanie daty ze stringa jest konceptem, który ma miejsce prawie w każdym systemie, który obsługuje daty. Początkowo, w wielu językach programowania, parsowanie to było dość skomplikowane. Gleam, jako nowoczesny język funkcjonalny, czerpie z doświadczeń tych starszych języków i ma na celu ułatwienie tego zadania.

Jeżeli chodzi o alternatywy, funkcja `StringToDate.string_to_date/1` nie jest jedyną dostępną funkcją do parsowania daty w Erlangu. Istnieją również inne biblioteki, takie jak `calendar` czy `timex`, które oferują o wiele więcej funkcjonalności. 

Wszystko to jest możliwe dzięki interopcji Gleam z Erlangiem. Dzięki temu można wykorzystywać bogatą bibliotekę funkcji Erlangu w naszych programach napisanych w Gleam.

## Zobacz Również:

- Dokumentacja Gleam: [https://gleam.run/docs/introduction/](https://gleam.run/docs/introduction/)
- Kurs Erlanga o parsowaniu daty: [https://learnyousomeerlang.com/time](https://learnyousomeerlang.com/time)
- Moduł Erlang time: [https://erlang.org/doc/man/time.html](https://erlang.org/doc/man/time.html)