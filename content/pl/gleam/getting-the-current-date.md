---
title:    "Gleam: Pobieranie aktualnej daty"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobranie aktualnej daty jest niezbędnym elementem w wielu programach i aplikacjach. Może ona być wykorzystana do wyświetlania daty, obliczania różnych interwałów czasowych lub manipulowania datami w różny sposób. W tym blogu przedstawimy, jak można w łatwy sposób uzyskać aktualną datę w języku programowania Gleam.

## Jak to zrobić

```Gleam
current_date = Date.now()
```

W powyższym przykładzie użyliśmy funkcji `now()` z modułu `Date`, aby uzyskać aktualną datę i przypisaliśmy ją do zmiennej `current_date`. Następnie możemy wykorzystać tę zmienną w programie, na przykład wyświetlając ją w konsoli:

```Gleam
IO.inspect(current_date)
```

Przykładowy output:

```
#<DateTime 2021-05-16T18:35:49.879970Z>
```

Funkcja `now()` zwraca wartość typu `DateTime`, która zawiera informacje o dacie i czasie wraz z informacją o strefie czasowej.

## Deep Dive

W języku Gleam istnieje kilka innych funkcji dostępnych w module `Date`, które można wykorzystać do manipulowania datami.

```Gleam
days_till_new_year = Date.diff(Date.now(), Date.from_tuple({2022, 1, 1}))
```

W powyższym przykładzie możemy obliczyć ilość dni pozostałych do nowego roku, wykorzystując funkcję `diff()`, która oblicza różnicę między dwoma datami. Wykorzystaliśmy również funkcję `from_tuple()`, aby utworzyć datę na podstawie krotki zawierającej rok, miesiąc i dzień.

## Zobacz też

- Dokumentacja modułu `Date` w języku Gleam: https://gleam.run/modules/date.html
- Poradnik o manipulacji datami w Gleam: https://dev.to/gleam_lang/date-manipulation-in-gleam-with-datetime-handling-errors-gracefully-4ha8