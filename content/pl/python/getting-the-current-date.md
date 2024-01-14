---
title:                "Python: Uzyskiwanie aktualnej daty"
simple_title:         "Uzyskiwanie aktualnej daty"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w naszym codziennym życiu potrzebujemy aktualnej daty, niezależnie czy pracujemy jako programiści, czy też jesteśmy po prostu zapracowanymi osobami, które muszą organizować swoje obowiązki. W tym artykule dowiesz się dlaczego pobieranie aktualnej daty jest ważne w programowaniu.

## Jak to zrobić?

Aby pobrać aktualną datę w języku Python, użyjemy modułu "datetime". Najpierw zaimportujemy ten moduł do naszego kodu:

```Python
import datetime
```

Następnie możemy użyć metody "now()" aby pobrać aktualną datę i czas:

```Python
current_date = datetime.datetime.now()

print(current_date)
```

**Output:**

```
2021-04-20 12:00:00.123456
```

Aby wyświetlić tylko datę lub czas, możemy użyć odpowiednich metod "date()" lub "time()":

```Python
print(current_date.date()) # wyświetli tylko datę

print(current_date.time()) # wyświetli tylko czas
```

**Output:**

```
2021-04-20 # tylko data
12:00:00.123456 # tylko czas
```

Możemy również wyświetlić tylko określone elementy daty lub czasu, na przykład:

```Python
print(current_date.year) # wyświetli rok

print(current_date.month) # wyświetli miesiąc

print(current_date.day) # wyświetli dzień miesiąca

print(current_date.hour) # wyświetli godzinę
```

**Output:**

```
2021 # rok
4 # miesiąc
20 # dzień miesiąca
12 # godzina
```

## Głębsza analiza

Moduł "datetime" posiada wiele więcej metod i funkcji, dzięki którym możemy manipulować datami i czasem w naszym kodzie. Możemy na przykład dodawać lub odejmować dni, godziny lub sekundy od aktualnej daty, dzięki czemu możemy tworzyć różne funkcje, jak na przykład kalendarze lub zegarki. Możemy również porównywać daty, sprawdzać czy jest to rok przestępny, łączyć różne elementy daty w jeden obiekt i wiele więcej.

## Zobacz również

- Dokumentacja Python dotycząca modułu "datetime": https://docs.python.org/3/library/datetime.html
- Wideo instruktażowe na YouTube o pobieraniu aktualnej daty w Python: https://www.youtube.com/watch?v=qjkR3jZ60gs
- Przydatny artykuł na temat manipulacji datami w Python: https://medium.com/@karambelkar/manipulating-dates-and-times-in-python-c212ea4f58a9