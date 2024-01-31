---
title:                "Анализ даты из строки"
date:                  2024-01-29T00:00:13.189010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"

category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор даты из строки означает преобразование текста в объект даты. Это делается потому, что с датами легче работать, вычислять разницы или форматировать их, когда они не застряли в виде обычного текста.

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
