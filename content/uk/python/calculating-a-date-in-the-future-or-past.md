---
title:    "Python: Обчислення дати у майбутньому або минулому"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Чому

Розрахунок дат у майбутньому та минулому може бути корисним для планування подій, висвітлення історичних дат або для швидкого знаходження різниці між двома датами.

## Як

Для розрахунку дат у майбутньому або минулому використовується модуль `datetime` у Python. Цей модуль містить функції та методи для роботи з датами та часом.

Наприклад, щоб додати 5 днів до поточної дати, використовуйте наступний код:

```Python
import datetime
today = datetime.date.today()
new_date = today + datetime.timedelta(days=5)
print(new_date)
```

Виведення буде таким: `2020-08-28 00:00:00`.

Щоб відняти 10 днів від поточної дати, скористайтеся таким кодом:

```Python
import datetime
today = datetime.date.today()
new_date = today - datetime.timedelta(days=10)
print(new_date)
```

Виведення буде таким: `2020-08-13 00:00:00`.

## Глибока поглиблення

Модуль `datetime` дозволяє також виконувати більш складні розрахунки дат. Наприклад, можна визначити різницю між двома датами, використовуючи метод `date`:

```Python
import datetime
date_1 = datetime.date(2020, 8, 28)
date_2 = datetime.date(2019, 8, 28)
difference = date_1 - date_2
print(difference.days)
```

Виведення буде таким: `365`.

Також можна порахувати зсув у днях за допомогою методу `timedelta`:

```Python
import datetime
date_1 = datetime.date(2020, 8, 20)
date_2 = datetime.date(2020, 8, 25)
difference = date_2 - date_1
print(difference.days)
```

Виведення буде таким: `5`.

## Дивись також

- [Офіційна документація з модуля datetime](https://docs.python.org/3/library/datetime.html)
- [Стаття "Робота з датами та часом в Python"](https://uk.pycon.org/2018/programme/talks/working-with-dates-and-time-in-python/)
- [Відеоурок "Робота з датами у Python"](https://www.youtube.com/watch?v=6wDb2rH8RYE)