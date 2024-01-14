---
title:    "Python: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być przydatne w wielu różnych sytuacjach. Na przykład, może Ci się przydać obliczenie daty urodzin dla przyjaciela lub ustalenie, kiedy kończy się termin ważności umowy. 

## Jak to zrobić

Obliczenie daty w przyszłości lub przeszłości w języku Python jest bardzo proste. Wystarczy użyć modułu `datetime` i kilku prostych funkcji.

```Python
# Importowanie modułu datetime
import datetime

# Obliczanie daty w przyszłości
dzien_przyszlosc = datetime.date.today() + datetime.timedelta(days=7)
print(dzien_przyszlosc)

# Obliczanie daty w przeszłości
dzien_przeszlosc = datetime.date.today() - datetime.timedelta(days=30)
print(dzien_przeszlosc)
```

W powyższym kodzie, najpierw importujemy moduł `datetime`, który jest wbudowany w język Python. Następnie używamy funkcji `date.today()`, aby pobrać obecną datę, a następnie dodajemy lub odejmujemy odpowiednią ilość dni za pomocą funkcji `timedelta()`. Na koniec używamy funkcji `print()`, aby wyświetlić obliczoną datę.

## Deep Dive

Moduł `datetime` oferuje wiele innych funkcji, które mogą być przydatne przy obliczaniu dat w przyszłości lub przeszłości. Na przykład, możesz użyć funkcji `strftime()` do sformatowania daty tak, jak chcesz. Możesz również obliczyć daty w innych jednostkach, takich jak miesiące lub lata, używając odpowiedniego parametru w funkcji `timedelta()`.

## Zobacz też

- [Dokumentacja Pythona dla modułu datetime] (https://docs.python.org/3/library/datetime.html)
- [Tutorial o obliczaniu dat w Pythonie] (https://realpython.com/python-datetime/)
- [Poradnik o formatowaniu dat w Pythonie] (https://www.datacamp.com/community/tutorials/python-datetime-tutorial)