---
title:                "Python: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulacja daty w przeszłości lub przyszłości może być przydatna w wielu sytuacjach, na przykład w planowaniu wydarzeń lub terminów ważnych zadań.

## Jak to zrobić?

Aby obliczyć datę w przeszłości lub przyszłości, musimy użyć modułu `datetime` w Pythonie. Najpierw musimy zaimportować ten moduł używając `import datetime`. Następnie możemy użyć funkcji `date()` by utworzyć obiekt daty z podanego roku, miesiąca i dnia. Przykładowo, `datetime.date(2021,7,10)` utworzy obiekt daty dla 10 lipca 2021 roku.

Teraz, aby dodać lub odjąć dni lub lata do tej daty, możemy użyć `timedelta` wraz z funkcją `date()`. Przykładowo, aby dodać 30 dni do daty, możemy użyć `datetime.date(2021,7,10) + datetime.timedelta(days=30)`. Funkcja ta zwróci datę 10 sierpnia 2021 roku.

Poniżej znajdują się przykładowe kodowe bloki w Pythonie oraz odpowiadające im wyniki:

```Python
import datetime

# Utworzenie obiektu daty dla 10 stycznia 2021 roku
date = datetime.date(2021,1,10)

# Dodanie 30 dni do daty
new_date = date + datetime.timedelta(days=30)

# Wyświetlenie obu dat
print("Stara data:", date)
print("Nowa data:", new_date)
```

```
Stara data: 2021-01-10
Nowa data: 2021-02-09
```

## Głębsze zanurzenie

W powyższym przykładzie, użyliśmy funkcji `timedelta` do dodania dni do naszej daty. Jednak, możemy również użyć tej funkcji do dodawania lub odejmowania lat, godzin, minut oraz sekund. Aby zapoznać się z pełnymi możliwościami tej funkcji, możesz zajrzeć do dokumentacji Pythona na temat modułu `datetime` i funkcji `timedelta`.

## Zobacz też

- Dokumentacja Pythona dotycząca modułu datetime: https://docs.python.org/3/library/datetime.html
- Inne przykłady wykorzystania dat w Pythonie: https://www.programiz.com/python-programming/datetime