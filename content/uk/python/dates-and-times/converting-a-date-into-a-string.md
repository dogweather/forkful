---
date: 2024-01-20 17:37:37.492194-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0417\u0430\u0440\u0430\u0437 \u043F\u043E\u0431\u0430\u0447\u0438\u043C\u043E\
  , \u044F\u043A \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0443\u0432\u0430\u0442\
  \u0438 \u0434\u0430\u0442\u0443 \u0443 \u0440\u044F\u0434\u043E\u043A \u0443 Python."
lastmod: '2024-03-13T22:44:48.603035-06:00'
model: gpt-4-1106-preview
summary: "\u0417\u0430\u0440\u0430\u0437 \u043F\u043E\u0431\u0430\u0447\u0438\u043C\
  \u043E, \u044F\u043A \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0443\u0432\u0430\
  \u0442\u0438 \u0434\u0430\u0442\u0443 \u0443 \u0440\u044F\u0434\u043E\u043A \u0443\
  \ Python."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

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
