---
title:                "Python: Робота з json"
simple_title:         "Робота з json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Робота з форматом JSON є дуже корисною для програмістів, які працюють зі структурованими данними. Використання JSON дозволяє зберігати та обмінюватися даними між різними програмами та системами без проблем з обробкою різних форматів.

## Як працювати з JSON у Python

Щоб почати роботу з JSON у Python, спочатку необхідно імпортувати модуль `json`:

```Python
import json
```

Потім можна створити змінну та задати у неї дані у форматі JSON:

```Python
data = {"name": "John", "age": 27, "city": "Kyiv"}
```

Для перетворення цих даних у рядок у форматі JSON використовуємо функцію `dumps()`:

```Python
json_data = json.dumps(data)
```

Остаточно, ми можемо перетворити отриманий рядок назад у формат словника Python, використовуючи функцію `loads()`:

```Python
new_data = json.loads(json_data)
```

В результаті ми отримаємо той самий словник, з якого почали. Також, можливо обробляти дані з файлу у форматі JSON:

```Python
with open("data.json", "r") as f:
    json_data = json.load(f)
```

## Глибоке занурення

Розглянемо деякі корисні функції та методи, що допоможуть працювати з JSON у Python:

- `json.dumps()` - перетворює дані у форматі Python у рядок у форматі JSON;
- `json.loads()` - перетворює рядок у форматі JSON у формат словника Python;
- `json.dump()` - записує дані у форматі JSON у зазначений файл;
- `json.load()` - читає дані у форматі JSON з зазначеного файлу;
- `json.dumps(..., indent=n)` - додає відступи у `n` пробілів для кожного рівня вкладених об'єктів;
- `json.dumps(..., ensure_ascii=False)` - дозволяє виводити не-ASCII символи у файл.

## Дивись також

- [Документація Python для модуля `json`](https://docs.python.org/3/library/json.html)
- [Інструкція по роботі з JSON у Python](https://realpython.com/python-json/)
- [Використання формату JSON у веб-розробці](https://www.w3schools.com/js/js_json_intro.asp)