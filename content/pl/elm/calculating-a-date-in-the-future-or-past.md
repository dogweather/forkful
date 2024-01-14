---
title:    "Elm: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego?

Obliczanie daty w przyszłości lub przeszłości może być przydatne w wielu aplikacjach, na przykład przy planowaniu wydarzeń lub przypomnieniach. W języku Elm istnieje wiele sposobów na wykonanie takiej operacji, a w tym artykule dowiesz się jak to zrobić.

# Jak to zrobić?

Pierwszym krokiem jest zaimportowanie modułu `Time`. Następnie można wykorzystać funkcję `Time.offsetInDays`, która przyjmuje dwa argumenty: datę początkową oraz liczbę dni, o jaką należy przesunąć datę. Przykładowy kod wyglądać będzie następująco:

```elm
import Time

startDate = Time.millisToPosix 0

futureDate = Time.offsetInDays startDate 10

pastDate = Time.offsetInDays startDate -5
```

W powyższym przykładzie użyto funkcji `millisToPosix` do przekonwertowania liczby milisekund na datę w formacie `Posix`, które jest używane przez funkcję `Time.offsetInDays`. Należy jednak pamiętać, że strefa czasowa w której działa aplikacja również wpływa na wynik, dlatego należy dokładnie sprawdzić dokumentację dla funkcji `millisToPosix`.

# Głębszy wgląd

Obliczanie daty w przyszłości lub przeszłości jest tylko jednym z wielu zadań, które można wykonać przy pomocy modułu `Time` w Elmie. Warto zapoznać się również z innymi funkcjami, takimi jak `Time.now`, `Time.utc`, czy `Time.compare`.

# Zobacz także
- Dokumentacja modułu `Time`: https://package.elm-lang.org/packages/elm/time/latest/
- Przykłady wykorzystania modułu `Time`: https://elmprogramming.com/dates-and-times.html
- Poradnik na temat dat w Elmie: https://guide.elm-lang.org/interop/dates_and_times.html