---
title:                "Отримання поточної дати"
aliases:
- /uk/python/getting-the-current-date/
date:                  2024-02-03T19:10:52.805750-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Отримання поточної дати в Python є стандартною операцією для багатьох застосунків, таких як логування, аналіз даних, і прийняття рішень на основі часу. Це стосується отримання поточної дати системи, що є критично важливим для завдань, залежних від часового контексту.

## Як:

**Використання стандартної бібліотеки `datetime`:**

Модуль `datetime` в стандартній бібліотеці Python надає класи для маніпулювання датами та часом. Для отримання поточної дати, ви можете використати метод `date.today()`.

```python
from datetime import date

today = date.today()
print(today)  # Вивід: РРРР-ММ-ДД (напр., 2023-04-05)
```

**Форматування часу:**

Якщо вам потрібна поточна дата в іншому форматі, метод `strftime` дозволяє вказати власне форматування дати:

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # Приклад формату: "Квітень 05, 2023"
print(formatted_date)
```

**Використання `pendulum` для більшої гнучкості (популярна стороння бібліотека):**

`Pendulum` — це стороння бібліотека, яка пропонує більш інтуїтивний підхід до роботи з датами та часом у Python. Вона розширює стандартні функціональні можливості datetime та спрощує управління часовими зонами, серед іншого.

Спочатку, переконайтеся, що ви встановили `pendulum` через pip:

```shell
pip install pendulum
```

Потім, щоб отримати поточну дату:

```python
import pendulum

today = pendulum.now().date()
print(today)  # Вивід: РРРР-ММ-ДД (напр., 2023-04-05)
```

З `pendulum`, форматування також просте та схоже на підхід `strftime`:

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # За замовчуванням формат: "Кві 5, 2023"
print(formatted_date)
```
