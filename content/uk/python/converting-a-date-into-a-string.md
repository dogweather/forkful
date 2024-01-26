---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:37:37.492194-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що & Навіщо?
Конвертація дати у рядок перетворює об'єкти дат і часу у зрозумілі та легко читаємі формати тексту, корисні для виводу, зберігання та обміну даними. Програмісти роблять це для гнучкості форматування та інтеграції з іншими системами.

## Як це зробити:
Зараз побачимо, як конвертувати дату у рядок у Python.

```Python
from datetime import datetime

# Текуща дата і час
now = datetime.now()

# Конвертація в рядок за замовчуванням
default_str = now.strftime("%Y-%m-%d %H:%M:%S")
print(default_str)  # "2023-04-01 12:30:45"

# Конвертація в різні формати
date_str = now.strftime("%d/%m/%Y")
time_str = now.strftime("%H:%M:%S")
print(date_str)  # "01/04/2023"
print(time_str)  # "12:30:45"
```

## Поглиблений огляд
Функція `strftime` є основою конвертації дат у рядки в Python, і вона приходить з C бібліотеки часу. Синтаксис форматування дат повторює стандарти C, що забезпечує сумісність між мовами. 

Альтернативи - це модулі третіх сторін, наприклад, `arrow` та `pendulum`, що пропонують зручніші інтерфейси і додаткові функції. 

Важливий момент - локалізація. Щоб виводити дату в українському форматі, можна встановити локаль:

```Python
import locale
locale.setlocale(locale.LC_TIME, 'uk_UA')

localized_str = now.strftime("%A, %d %B %Y р.")
print(localized_str)  # "Субота, 01 Квітень 2023 р."
```

Увага: Локаль потрібно підтримувати на вашій системі, інакше ви отримаєте помилку.

## Дивіться також
- Документація по модулю `datetime` в Python: https://docs.python.org/3/library/datetime.html
- Документація по форматам часу в strftime: https://strftime.org/
- Модуль `arrow` для Python: https://arrow.readthedocs.io/
- Модуль `pendulum` для Python: https://pendulum.eustace.io/
