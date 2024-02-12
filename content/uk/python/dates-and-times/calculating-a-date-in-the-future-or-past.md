---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/python/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:48.892708-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Обчислення дати в майбутньому чи минулому — це визначення конкретного дня відносно сьогодення. Програмісти це роблять, щоби виконувати планування, вести історії змін, встановлювати терміни та розділяти часові інтервали.

## Як це робити:
```Python
from datetime import datetime, timedelta

# Сьогоднішня дата
today = datetime.now()

# Додати 10 днів
future_date = today + timedelta(days=10)
print(f"Дата у майбутньому: {future_date.strftime('%Y-%m-%d')}")

# Відняти 5 днів
past_date = today - timedelta(days=5)
print(f"Дата у минулому: {past_date.strftime('%Y-%m-%d')}")
```
**Вивід:**
```
Дата у майбутньому: 2023-04-17
Дата у минулому: 2023-04-02
```
## Поглиблено:
В минулому програмісти використовували модулі C, різноманітні алгоритми та таблиці для обчислення дат. Зараз є стандартний модуль `datetime`, який значно спрощує завдання. 

Існують альтернативи `datetime`, наприклад, бібліотеки `dateutil`, `arrow` та `pendulum`. Вони надають більший функціонал для работы з часовими зонами, розбором рядків з датами, і відносними обчисленнями (наприклад, "наступний понеділок").

Обчислення в майбутнє та минуле працюють шляхом додавання або віднімання `timedelta` об'єктів до `datetime` об'єктів. `timedelta` може представляти дні, секунди, мікросекунди та навіть тижні.

## Дивись також:
- Документація `datetime`: https://docs.python.org/3/library/datetime.html
- `dateutil` бібліотека: https://dateutil.readthedocs.io/en/stable/
- `arrow` бібліотека: https://arrow.readthedocs.io/en/latest/
- `pendulum` бібліотека: https://pendulum.eustace.io/
