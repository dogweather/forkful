---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Python: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przeszłości lub przyszłości to proces wykorzystywany przez programistów do obliczania dat w sposób automatyczny. Może to być przydatne w wielu aplikacjach, na przykład w systemach rezerwacji lub harmonogramów. Programiści korzystają z tego narzędzia, aby uniknąć manualnego obliczania dat, co może być czasochłonne i podatne na błędy.

## Jak to zrobić:
```
Python ...

# Obliczenie daty 30 dni w przyszłości
future_date = datetime.date.today() + datetime.timedelta(days=30)
print(future_date)

# Obliczenie daty 30 dni w przeszłości
past_date = datetime.date.today() - datetime.timedelta(days=30)
print(past_date)

```

Wyjście:
```
2021-09-05
2021-07-06
```

## Głębszy zanurzenie:
Obliczanie dat jest wykorzystywane już od dawna przez programistów i było szczególnie przydatne w systemach rezerwacji biletów lub spłat kredytów, gdzie wyznaczenie przyszłych dat jest niezbędne. Istnieją również inne metody wykonywania tych obliczeń, takie jak korzystanie z bibliotek zewnętrznych lub implementacja własnej funkcji w Pythonie.

## Zobacz również:
Niektóre przydatne źródła dotyczące obliczania dat w przeszłości lub przyszłości:

- Dokumentacja Pythona: https://docs.python.org/3/library/datetime.html
- Wprowadzenie do wykorzystania biblioteki datetime w Pythonie: https://realpython.com/python-datetime/
- Porównanie różnych metod obliczania dat w Pythonie: https://www.geeksforgeeks.org/python-date-operations-compare-dates/