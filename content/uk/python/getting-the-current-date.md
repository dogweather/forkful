---
title:                "Python: Отримання поточної дати"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні часто потрібно отримати поточну дату. Це може бути корисно для ведення логів, створення звітів або для визначення часу виконання певної операції. Якщо ви створюєте скрипт, який буде виконуватися автоматично, бажано знати, коли саме він був запущений.

## Як це зробити

Щоб отримати поточну дату в Python, ви можете використовувати модуль `datetime`. Існує декілька способів отримати поточну дату з цим модулем. Давайте розглянемо декілька з них:

```Python
import datetime

# Отримати поточну дату
today = datetime.date.today()
print(today)

# Отримати поточний рік
year = datetime.date.today().year
print(year)

# Отримати поточний місяць
month = datetime.date.today().month
print(month)

# Отримати поточний день
day = datetime.date.today().day
print(day)
```

Приклад виводу:

```
2021-08-31
2021
08
31
```

## Deep Dive

Модуль `datetime` надає багато корисних функцій для роботи з датами і часом. Ви можете отримати не тільки поточну дату, але і будь-яку іншу визначену дату, встановити час, перетворити формат дати та багато іншого. Детальніше про цей модуль ви можете дізнатися з [офіційної документації](https://docs.python.org/3/library/datetime.html).

## Дивіться також

- [Форматування дати і часу в Python](https://www.datacamp.com/community/tutorials/python-formatted-datetime)
- [Робота з датами і часом у Python](https://realpython.com/python-datetime/)
- [Робота з часовими зонами в Python](https://www.geeksforgeeks.org/python-working-with-time-zones/)