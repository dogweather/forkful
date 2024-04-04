---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Python \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u043F\u0440\
  \u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\u0430\
  \u0442 \u0432 \u0441\u0442\u0440\u043E\u043A\u0438. \u0418\u0441\u043F\u043E\u043B\
  \u044C\u0437\u0443\u0439\u0442\u0435 \u043C\u0435\u0442\u043E\u0434 [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-\u2026"
lastmod: '2024-04-04T02:02:44.853941-06:00'
model: gpt-4-0125-preview
summary: "Python \u0443\u043F\u0440\u043E\u0449\u0430\u0435\u0442 \u043F\u0440\u0435\
  \u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u0434\u0430\u0442\
  \ \u0432 \u0441\u0442\u0440\u043E\u043A\u0438."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как это сделать:
Python упрощает преобразование дат в строки. Используйте метод [`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior), доступный для объектов типа [date](https://docs.python.org/3/library/datetime.html#date-objects). Вот как это делается:

```Python
from datetime import datetime

# Получаем текущие дату и время
now = datetime.now()

# Преобразуем в строку в формате: Месяц день, Год
date_string = now.strftime("%B %d, %Y")
print(date_string)  # Вывод: Март 29, 2023 (или текущая дата)

# Формат: ГГГГ-ММ-ДД
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # Вывод: 2023-03-29 (или текущая дата)
```


### Как я это делаю

Вот как я получаю дату в формате [ISO 8601](https://www.w3.org/QA/Tips/iso-date) с информацией о часовом поясе:

```python
def datestamp() -> str:
    """ 
    Текущая дата и время с часовым поясом в формате ISO.
    """
    return datetime.now().astimezone().isoformat()
```

#### Пример вывода:

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## Подробно
Исторически преобразование даты в строку является ключевым моментом в программировании из-за необходимости представления дат в формате, понятном людям.

Альтернативы `strftime` включают использование метода `isoformat` для формата ISO 8601 или сторонних библиотек, таких как `arrow` и `dateutil`, которые предлагают более гибкие возможности анализа и форматирования.

С точки зрения реализации, `strftime` означает "форматирование времени в строку" и имеет корни в программировании на C. `strftime` в Python интерпретирует коды формата, такие как `%Y` для года и `%m` для месяца, что позволяет получать практически неограниченные возможности для настройки.

## Смотрите также
Чтобы углубиться в функции даты и времени Python:
- Официальная документация по `datetime` Python: https://docs.python.org/3/library/datetime.html
- Для тех, кто заинтересован в полном списке директив `strftime`: https://strftime.org/
- Для изучения сторонних библиотек работы с датой/временем:
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
