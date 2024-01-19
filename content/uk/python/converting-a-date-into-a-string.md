---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і для чого?

Конвертація дати в рядок в Python - це процес перетворення об'єкта дати на рядок. Це зручно для відображення дати в певному форматі або для зберігання в текстовій формі.

## Як це зробити:

В Python для конвертації дати в рядок ми використаємо функцію `strftime()`. Ось приклад:

```python
from datetime import datetime

# створимо об'єкт дати
date = datetime(2025, 7, 4)

# конвертуємо дату в рядок
date_string = date.strftime("%d.%m.%Y")

print(date_string)
```

Вихідний результат:
```
"04.07.2025"
```

## Поглиблений огляд:

`strftime()` - це функція, що була введена ще в Python 1.5.2 і вона являє собою вбудований метод у Python, який дозволяє форматувати об'єкти datetime як рядки.

Саме `strftime()` є найрозповсюдженішим методом для конвертування дат в рядки. Проте можна також використовувати функцію `isoformat()` для отримання рядка дати у форматі ISO 8601.

Важливо зазначити, що `strftime()` забезпечує гнучкість у визначенні формату вихідного рядка за допомогою різноманітних директив форматування, таких як "%Y" для року або "%d" для дня місяця.

## Дивіться також:

1. Python `datetime` documentation: https://docs.python.org/3/library/datetime.html

2. Python String Format Code for `strftime()`: https://strftime.org/

3. Python Formatter class: https://docs.python.org/3/library/string.html#format-string-syntax