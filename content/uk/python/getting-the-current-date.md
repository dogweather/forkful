---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:16:27.778828-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
Отримати поточну дату - це значить запитати у вашої системи "який сьогодні день?" Це роблять, щоб маркувати час створення файлів, логування подій чи обмежити доступ до ресурсу за часом.

## Як це зробити:
```Python
from datetime import datetime

# Отримати поточну дату і час
now = datetime.now()

# Вивести поточну дату і час
print("Дата та час зараз:", now)

# Вивести тільки дату
print("Тільки сьогоднішня дата:", now.date())

# Вивести в форматі рррр-мм-дд
print("Форматована дата:", now.strftime("%Y-%m-%d"))
```

Вивід:
```
Дата та час зараз: 2023-04-02 15:37:21.091417
Тільки сьогоднішня дата: 2023-04-02
Форматована дата: 2023-04-02
```

## Поглиблений огляд:
Отримання поточної дати має коріння в операційних системах і часах початкових комп'ютерів. Це пов'язано з потребою відстежування подій. Альтернативи `datetime` в Python включають модулі `time` та `calendar`. `datetime` має методи для аналізу, форматування і проведення операцій з датами. Использование `timezone` важливо для точності з урахуванням часових поясів.

## Також дивіться:
- [Документація по модулю datetime](https://docs.python.org/3/library/datetime.html)
- [Документація по модулю time](https://docs.python.org/3/library/time.html)
- [PEP 495 -- Local Time Disambiguation](https://www.python.org/dev/peps/pep-0495/)
- [ISO 8601 Date and time format](https://www.iso.org/iso-8601-date-and-time-format.html)
