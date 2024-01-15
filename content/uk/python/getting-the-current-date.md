---
title:                "Отримання поточної дати"
html_title:           "Python: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Бувають ситуації, коли нам потрібно отримати поточну дату в програмі. Це може бути корисно для створення логів, розрахунків часу або у будь-якому іншому випадку, коли ми потребуємо точних даних про поточний день.

## Як отримати поточну дату в Python

```Python
# Імпортуємо модуль datetime
import datetime

# Отримуємо поточну дату
current_date = datetime.date.today()

# Виводимо на екран
print(current_date)
```

В результаті виконання цього коду ми отримаємо поточну дату у форматі YYYY-MM-DD. Можна також отримати окремо рік, місяць або день за допомогою методів `year`, `month` і `day`.

```Python
# Отримуємо поточний рік
current_year = datetime.date.today().year

# Отримуємо поточний місяць
current_month = datetime.date.today().month

# Отримуємо поточний день
current_day = datetime.date.today().day

# Виводимо окремо на екран
print(current_year, current_month, current_day)
```

## Глибоке дослідження

Модуль `datetime` в Python надає багато інших функцій для роботи з датами і часом. Наприклад, за допомогою класу `timedelta` можна виконувати арифметичні операції з датами. Також можна отримати поточний час або час з точністю до мілісекунд за допомогою класу `datetime`.

Вивчення деталей цього модуля може бути корисним для роботи з датами і часом в програмах і скриптах на Python.

## Дивіться також

- [Документація по модулю datetime в Python](https://docs.python.org/3/library/datetime.html)
- [Стаття про роботу з датами і часом в Python](https://realpython.com/python-datetime/)
- [Поради та трюки по роботі з датами і часом в Python](https://www.datacamp.com/community/tutorials/python-datetime-tutorial)