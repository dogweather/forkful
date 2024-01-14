---
title:    "Python: Отримання поточної дати"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Чому

Отримання поточної дати - це важлива частина програмування, яка дозволяє нам працювати з часом в наших проектах. Це може бути корисно для ведення журналу подій, розрахунку таймінгів та багатьох інших задач.

# Як

```Python
import datetime
current_date = datetime.datetime.now() # Отримати поточну дату і час
current_date = current_date.date() # Витягнути тільки дату
current_time = current_date.time() # Витягнути тільки час
print(current_date) # 2021-08-16
print(current_time) # 09:30:00
```

# Поглиблене вивчення

У модулі datetime існує багато функцій для роботи з датами та часом. Наприклад, ви можете використовувати функції `strftime()` та `strptime()` для форматування та розпарсування дати. Ви також можете розрахувати різницю між двома датами за допомогою функції `timedelta()`.

# Дивіться також

- [Документація по модулю datetime](https://docs.python.org/3/library/datetime.html)
- [Стаття про роботу з датами та часом в Python](https://realpython.com/python-datetime/)
- [Програмування на Python для початківців](https://uk.codeacademy.com/learn/learn-python)