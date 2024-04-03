---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:13.189010-07:00
description: "\u041A\u0430\u043A: \u041C\u043E\u0434\u0443\u043B\u044C `datetime`\
  \ \u0432 Python \u2014 \u0432\u0430\u0448 \u0433\u043B\u0430\u0432\u043D\u044B\u0439\
  \ \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442 \u0434\u043B\u044F\
  \ \u0440\u0430\u0437\u0431\u043E\u0440\u0430 \u0434\u0430\u0442. \u0412\u043E\u0442\
  \ \u043A\u0440\u0430\u0442\u043A\u043E\u0435 \u0440\u0443\u043A\u043E\u0432\u043E\
  \u0434\u0441\u0442\u0432\u043E."
lastmod: '2024-03-13T22:44:44.287987-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u043E\u0434\u0443\u043B\u044C `datetime` \u0432 Python \u2014 \u0432\
  \u0430\u0448 \u0433\u043B\u0430\u0432\u043D\u044B\u0439 \u0438\u043D\u0441\u0442\
  \u0440\u0443\u043C\u0435\u043D\u0442 \u0434\u043B\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440\u0430 \u0434\u0430\u0442."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как:
Модуль `datetime` в Python — ваш главный инструмент для разбора дат. Вот краткое руководство:

```python
from datetime import datetime

date_string = "2023-04-01"
date_object = datetime.strptime(date_string, "%Y-%m-%d")

print(date_object)  # Вывод: 2023-04-01 00:00:00

# Хотите увидеть другой формат? Давайте попробуем "день-месяц-год".
another_date_string = "01-April-2023"
another_date_object = datetime.strptime(another_date_string, "%d-%B-%Y")

print(another_date_object)  # Вывод: 2023-04-01 00:00:00
```

## Подробнее
Разбор дат стал необходимым, когда базы данных и пользовательские интерфейсы начали тесно взаимодействовать. Исторически данные часто хранились в виде строк, включая даты. Теперь, однако, у нас есть модуль `datetime`, который был введен в Python версии 2.3 (и значительно улучшен с тех пор).

Вы не ограничены использованием `datetime`. Можно использовать сторонние библиотеки, такие как `dateutil`, которая более снисходительна к форматам, или `pandas` для серьезной работы с анализом данных.

С точки зрения реализации `strptime` означает "разбор строки времени" и использует коды форматирования для распознавания шаблонов. Это означает, что вы должны сообщить Python формат строки с датой, например, `%Y` для четырехзначного года или `%d` для дня.

## Смотрите также
- Документация по datetime: https://docs.python.org/3/library/datetime.html
- Парсер dateutil: https://dateutil.readthedocs.io/en/stable/parser.html
- Функция to_datetime в Pandas: https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.to_datetime.html
