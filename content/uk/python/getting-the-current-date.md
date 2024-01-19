---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Python: Як отримати поточну дату?

## Що і чому?

Отримання поточної дати в Python - це швидкий спосіб знати дату і час прямо зараз. Програмісти використовують це для логічних рішень, аудиту активності, відстеження часу виконання програми, чи теж для штампів дати/часу.

## Як зробити:

Модуль datetime в Python включає всі основні операції з датою і часом. Ось приклад отримання поточної дати:

```Python
import datetime

current_date = datetime.date.today()
print(current_date)
```

Виходом буде поточна дата у форматі YYYY-MM-DD (наприклад, 2022-09-25).

## Поглиблений огляд:

**Історичний контекст**: У ранніх версіях Python для роботи з датою та часом були модулі time та calendar. Згодом було вирішено створити unified interface, тому було створено модуль datetime.

**Альтернативи**: Якщо вам потрібно більше функціональності для роботи з датою та часом, можна використовувати модулі, такі як Pendulum, arrow, або delorean.

**Деталі реалізації**: datetime використовує Gregorian calendar (такий самий, як у нас в усьому світі), тому враховуеться високосний рік і інші особливості такого календаря.

## Дивись також:

1. [Python's official documentation for datetime](https://docs.python.org/3/library/datetime.html)
2. [Arrow: Python library for date & time manipulation](https://arrow.readthedocs.io/en/latest/)
3. [How to convert a date string into different formats in Python?](https://stackoverflow.com/questions/466345/converting-string-into-datetime)