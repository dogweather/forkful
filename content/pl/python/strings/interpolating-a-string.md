---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:23:59.921095-07:00
description: "Jak to zrobi\u0107: W Pythonie 3.6 i nowszych mo\u017Cna interpolowa\u0107\
  \ ci\u0105gi znak\xF3w u\u017Cywaj\u0105c f-string\xF3w. Oto jak."
lastmod: '2024-03-13T22:44:34.934628-06:00'
model: gpt-4-0125-preview
summary: "W Pythonie 3.6 i nowszych mo\u017Cna interpolowa\u0107 ci\u0105gi znak\xF3\
  w u\u017Cywaj\u0105c f-string\xF3w."
title: "Interpolacja \u0142a\u0144cucha znak\xF3w"
weight: 8
---

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
