---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Python \u0437\u043D\u0430\u0447\u043D\u043E \u0441\u043F\u0440\u043E\u0449\u0443\
  \u0454 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u043D\u0430 \u0440\u044F\u0434\u043A\u0438. \u0421\u043A\u043E\
  \u0440\u0438\u0441\u0442\u0430\u0439\u0442\u0435\u0441\u044C \u043C\u0435\u0442\u043E\
  \u0434\u043E\u043C\u2026"
lastmod: '2024-04-04T02:02:56.523907-06:00'
model: gpt-4-0125-preview
summary: "Python \u0437\u043D\u0430\u0447\u043D\u043E \u0441\u043F\u0440\u043E\u0449\
  \u0443\u0454 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u043D\u0430 \u0440\u044F\u0434\u043A\u0438."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## Як це зробити:
Python значно спрощує перетворення дат на рядки. Скористайтесь методом [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior), який доступний для об'єктів [date](https://docs.python.org/3/library/datetime.html#date-objects). Ось як:

```Python
from datetime import datetime

# Отримати поточну дату та час
now = datetime.now()

# Перетворити її на рядок у форматі: Місяць день, Рік
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Вивід: Березень 29, 2023 (або поточна дата)

# Формат: РРРР-ММ-ДД
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Вивід: 2023-03-29 (або поточна дата)
```


### Як я це роблю

Ось як я отримую дату в форматі [ISO 8601](https://www.w3.org/QA/Tips/iso-date) з інформацією про часовий пояс:

```python
def datestamp() -> str:
    """ 
    Поточна дата та час з часовим поясом у форматі ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Приклад виводу:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Глибше занурення
Історично, перетворення дат у рядки було основоположним в програмуванні через необхідність представлення дат у зручному для людини форматі. 

Альтернативи `strftime` включають використання методу `isoformat` для формату ISO 8601, або сторонніх бібліотек, таких як `arrow` і `dateutil`, які пропонують більш гнучкі опції парсингу та форматування.

З точки зору реалізації, `strftime` означає "string format time" (рядковий формат часу) і має коріння в програмуванні на C. `strftime` у Python інтерпретує коди форматування, такі як `%Y` для року і `%m` для місяця, дозволяючи майже нескінченну налаштовуваність.

## Дивіться також
Для глибшого занурення в функції дати та часу в Python:
- Офіційна документація Python `datetime`: https://docs.python.org/3/library/datetime.html
- Для тих, хто зацікавлений в повному списку директив `strftime`: https://strftime.org/
- Для дослідження сторонніх бібліотек дати/часу:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
