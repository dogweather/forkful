---
title:                "Otrzymywanie aktualnej daty"
html_title:           "Python: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Obecna data jest często potrzebna w programowaniu, ponieważ pozwala na utworzenie dynamicznych aplikacji, które wykorzystują aktualny czas. Może być również używana do tworzenia funkcji, które mają reagować na konkretny dzień lub czas, np. wyświetlenie innej informacji w święta.

## Jak to zrobić

```python
import datetime

aktualna_data = datetime.datetime.now()
print("Aktualna data i czas:", aktualna_data)
```
#### Wynik:
```
Aktualna data i czas: 2021-04-16 13:30:00.480041
```

Możemy również wyświetlić tylko konkretną część daty, np. dzień lub miesiąc, korzystając z odpowiednich funkcji, takich jak `day` i `month`.

```python
aktualna_data = datetime.datetime.now()
dzień = aktualna_data.day
miesiąc = aktualna_data.month
print("Dziś jest", dzień, "dzień miesiąca", miesiąc)
```
#### Wynik:
```
Dziś jest 16 dzień miesiąca 4
```

Możemy także użyć modułu `calendar` do wyświetlenia nazwy dnia tygodnia.

```python
import calendar
aktualna_data = datetime.datetime.now()
dzień_tygodnia = calendar.day_name[aktualna_data.weekday()]
print("Dziś jest", dzień_tygodnia)
```
#### Wynik:
```
Dziś jest piątek
```

## W głębi rzeczy

Aby dokładnie zrozumieć, jak działa funkcja `datetime.now()` należy wiedzieć, że zwraca ona obiekt typu `datetime`, który zawiera informacje o bieżącej dacie i czasie. Następnie możemy używać różnych metod na tym obiekcie, np. `day`, `month`, `year`, aby otrzymać pożądane informacje.

Moduł `calendar` zawiera wiele przydatnych funkcji, które pozwalają na łatwą obsługę dat i czasu. W powyższym przykładzie korzystaliśmy z `day_name` aby wyświetlić nazwę dnia tygodnia, ale istnieją również inne funkcje, takie jak `month` czy `week`, które mogą być wykorzystane w naszych aplikacjach.

## Zobacz także

- [Dokumentacja modułu datetime](https://docs.python.org/3/library/datetime.html)
- [Poradnik dla początkujących w języku Python - część 2](https://programiz.com/python-programming/datetime)
- [Podstawy funkcji dat i czasu w Pythonie](https://www.w3schools.com/python/python_datetime.asp)