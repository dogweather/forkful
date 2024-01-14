---
title:                "Python: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому
Розрахування дати в майбутньому або минулому може бути корисним у багатьох випадках, наприклад, для планування подій або розрахунку строків. Крім того, це важливо для веб-сайтів, які пропонують послуги доставки або бронювання.

## Як
Калькулювання дати в майбутньому або минулому можливе завдяки використанню модуля `datetime` у Python. Основними методами для роботи з датами є `strftime()` для форматування та `strptime()` для парсингу. Нижче наведений приклад коду, який демонструє розрахунок дати в майбутньому на основі введеного користувачем року:

```Python
from datetime import datetime

year = input("Введіть рік: ")
future_date = datetime.strptime(year, '%Y') + timedelta(days=365 * 5)

print("Дата через 5 років:", future_date.strftime('%d.%m.%Y'))
```

При введенні року 2020 програма виведе дату 31.12.2025.

## Глибокий занурення
Більш складні розрахунки дат можливі завдяки використанню різних функцій та методів модуля `datetime`. Наприклад, метод `timedelta()` дозволяє додавати або віднімати певну кількість днів, хвилин або секунд до введеної дати. Крім того, модуль `calendar` надає можливість отримати день тижня для будь-якої дати.

## Дивись також
- [Документація Python для модуля `datetime`](https://docs.python.org/3/library/datetime.html)
- [Стаття про локалізацію дат у Python](https://tproger.ru/translations/timestamps-in-localized-formats-with-python-datetime/)
- [Порівняння дат у Python за допомогою модуля `dateutil`](https://stackabuse.com/comparing-dates-in-python-using-the-dateutil-module/)