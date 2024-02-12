---
title:                "Interpolacja łańcucha znaków"
date:                  2024-01-28T21:23:59.921095-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpolacja łańcucha znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
Interpolacja ciągów znaków to metoda osadzania wyrażeń wewnątrz literałów napisów. Programiści używają jej do dynamicznego wstawiania wartości do ciągów znaków, co sprawia, że kod jest bardziej czytelny i czystszy niż tradycyjna konkatenacja ciągów znaków.

## Jak to zrobić:
W Pythonie 3.6 i nowszych można interpolować ciągi znaków używając f-stringów. Oto jak:

```Python
name = 'Alice'
age = 30
greeting = f"Witaj, {name}. Masz {age} lat."

print(greeting)
```

Wynik:
```
Witaj, Alice. Masz 30 lat.
```

Możesz również użyć wyrażeń wewnątrz nawiasów klamrowych:

```Python
a = 5
b = 10
info = f"Pięć plus dziesięć to {a + b}, a nie {2 * (a + b)}."

print(info)
```

Wynik:
```
Pięć plus dziesięć to 15, a nie 30.
```

## Dogłębna analiza
Przed Pythonem 3.6, do interpolacji ciągów znaków używano metody `.format()`:

```Python
name = 'Bob'
age = 25
greeting = "Witaj, {}. Masz {} lat.".format(name, age)

print(greeting)
```

Stary Python (wersje < 2.6) używał operatora `%` do interpolacji, co jest mniej intuicyjne i może stać się bałaganem przy wielu zmiennych:

```Python
name = 'Carol'
age = 35
greeting = "Witaj, %s. Masz %d lat." % (name, age)

print(greeting)
```

Oprócz czystszej składni, f-stringi są szybsze, ponieważ są oceniane w czasie wykonania, a następnie bezpośrednio przekształcane w efektywną operację formatowania ciągu znaków. Metoda `.format()` i operator `%` wiążą się z większą liczbą kroków i są wolniejsze.

## Zobacz także
- [PEP 498 – Interpolacja Literałów Napisów](https://www.python.org/dev/peps/pep-0498/) dla oficjalnej dokumentacji f-stringów.
- [Python f-stringi](https://realpython.com/python-f-strings/) na Real Python dla tutorialu o używaniu f-stringów.
- [Metoda .format()](https://docs.python.org/3/library/stdtypes.html#str.format) w dokumentacji Pythona, aby zrozumieć starszą metodę formatowania napisów.
