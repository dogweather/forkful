---
title:    "Python: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że musimy obliczyć datę w przyszłości lub w przeszłości w naszym kodzie Python. Mogą to być ważne daty, takie jak urodziny, święta, terminy ważności, itp. Obliczenie tych dat jest nie tylko przydatne, ale również prostym sposobem na ulepszenie naszych programów.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w Pythonie, musimy najpierw zaimportować bibliotekę "datetime". Następnie możemy użyć funkcji datetime.timedelta(), która pozwala na dodawanie lub odejmowanie określonej liczby dni, tygodni, miesięcy lub lat do istniejącej daty.

```Python
import datetime

# Obliczanie daty w przyszłości
data = datetime.date(2020, 12, 31)
data_przyszla = data + datetime.timedelta(days=7)
print(data_przyszla)

# Output: 2021-01-07

# Obliczanie daty w przeszłości
data = datetime.date(2020, 12, 31)
data_przeszla = data - datetime.timedelta(weeks=2)
print(data_przeszla)

# Output: 2020-12-17 
```

Możemy również wykorzystać funkcję datetime.timedelta() do porównywania dat lub do obliczania różnicy między dwiema datami.

```Python
import datetime

# Porównywanie dat
data_1 = datetime.date(2020, 12, 31)
data_2 = datetime.date(2021, 1, 1)

if data_1 < data_2:
  print("Data 1 jest wcześniejsza niż data 2")

# Output: Data 1 jest wcześniejsza niż data 2

# Obliczanie różnicy między datami
data_1 = datetime.date(2020, 12, 31)
data_2 = datetime.date(2021, 1, 1)

roznica = data_2 - data_1
print(roznica.days)

# Output: 1
```

## Głębszy zanurzenie

Istnieje wiele innych funkcji w bibliotece "datetime", które pozwalają na bardziej precyzyjne obliczenia dat, takie jak uwzględnianie stref czasowych, godzin, minut, sekund, i wiele więcej. Jest to przydatne, gdy nasze obliczenia wymagają większej dokładności.

## Zobacz także
- [Dokumentacja biblioteki "datetime" w Pythonie](https://docs.python.org/3/library/datetime.html)
- [Poradnik "datetime" w Pythonie na stronie W3Schools](https://www.w3schools.com/python/python_datetime.asp)
- [Poradnik na temat obliczania dat w Pythonie na stronie Real Python](https://realpython.com/python-date-time/)