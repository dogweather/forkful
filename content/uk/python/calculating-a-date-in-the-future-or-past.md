---
title:                "Розрахунок дати в майбутньому або минулому"
html_title:           "Python: Розрахунок дати в майбутньому або минулому"
simple_title:         "Розрахунок дати в майбутньому або минулому"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Обчислення дати в майбутньому або минулому у Python

## Що та Навіщо?

Обчислення дати в майбутньому або минулому - це процес виявлення якоїсь конкретної дати на основі відомої дати та інтервалу часу. Програмісти роблять це для обробки та аналізу часових серій, планування подій або відстеження термінів.

## Як це робити:

В Python для цього зазвичай використовують модуль `datetime`. Давайте розглянемо декілька прикладів.

```Python
from datetime import datetime, timedelta

# Знайдемо дату, що буде через 7 днів
now = datetime.now()
future_date = now + timedelta(days=7)
print(future_date)
```
Цей код виведе дату, що буде через 7 днів від поточного моменту.

```Python
from datetime import datetime, timedelta

# Знайдемо дату, що була 7 днів назад
now = datetime.now()
past_date = now - timedelta(days=7)
print(past_date)
```
Цей код виведе дату, що була 7 днів назад від поточного моменту.

## Чим глибше, тим цікавіше

У Python існує декілька модулів, що можуть допомогти з обчисленням дат: `datetime`, `time`, `calendar`, і інші. Модуль `datetime` є найбільш загальною та гнучкою бібліотекою для роботи з датами і часом. Він був включений до стандартної бібліотеки Python у версії 2.3 у 2003 році.

Є також альтернативні бібліотеки, такі як `dateutil`, які забезпечують більш продвинуті функції. Наприклад, `dateutil` може обробляти повторювані події, залежні від timezone, і багато іншого.

Якщо ви використовуєте `datetime` для обчислення дати в майбутньому або минулому, будьте уважні з обробкою скоків ( високосних років та змін часу на літо/зиму) - це може змінити ваш результат на 1 день!

## Дивись також:

1. Документація Python по модулю datetime - <https://docs.python.org/3/library/datetime.html>
2. Про маєтеються проблеми з часовими зонами в Python - <https://www.youtube.com/watch?v=F6IThBKzY3Y>
3. Бібліотека dateutil - <https://dateutil.readthedocs.io/en/stable/>
4. Python Arrow Library - <https://arrow.readthedocs.io/en/latest/>