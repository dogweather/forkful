---
title:                "Gleam: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu musimy przekształcić datę na napis, na przykład, aby wyświetlić ją w czytelniejszy sposób lub przechowywać w bazie danych. W tym artykule dowiesz się, jak w języku Gleam wykonać tę operację.

## Jak to zrobić

W języku Gleam istnieje wiele narzędzi do obsługi dat, ale dla naszego celu będziemy korzystać z modułu `Date`. Najpierw musimy zaimportować ten moduł za pomocą słowa kluczowego `import`:

```Gleam
import Date
```

Następnie możemy użyć funkcji `to_string`, która przyjmuje dwa argumenty: `date` - obiekt daty, którą chcemy przekształcić oraz `options` - ustawienia formatowania. Na przykład, aby przekształcić bieżącą datę w formacie yyyy-mm-dd, można użyć następującego kodu:

```Gleam
let current_date = Date.now()
let date_string = Date.to_string(current_date, Date.Options.default)
```

Wynik powinien być w formacie `2021-08-15`. Możemy również użyć opcji `Date.Option.day_of_week` aby dodać do napisu dzień tygodnia. Wtedy wynik będzie wyglądał tak: `Sun, 15 Aug 2021`.

## Głębsza analiza

Jeśli chcesz dowiedzieć się więcej o funkcjach `Date`, możesz zajrzeć do dokumentacji języka Gleam lub przejrzeć kod źródłowy modułu `Date` na GitHubie.

## Zobacz również

- [Dokumentacja języka Gleam](https://gleam.run/documentation)
- [Kod źródłowy modułu Date na GitHubie](https://github.com/gleam-lang/gleam/blob/master/lib/date/src/date.gleam)