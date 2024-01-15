---
title:                "Робота з json"
html_title:           "Python: Робота з json"
simple_title:         "Робота з json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/working-with-json.md"
---

{{< edit_this_page >}}

## Чому 

В сьогоднішньому світі обмін даними є незамінним елементом програмування. Крім того, робота з даними в форматі JSON є швидшою та більш зручною, оскільки цей формат є стандартом для обміну даними в Інтернеті.

## Як

```Python
import json
 
# створення простого об'єкта JSON
user = {
  "name": "Анна",
  "age": 25,
  "city": "Київ"
}
 
# перетворення об'єкта в JSON строку
user_json = json.dumps(user)
print(user_json)

# виведення значення конкретного ключа
print(user["name"])

# розбирання JSON строку в об'єкт Python
car_string = '{"brand": "Mercedes", "model": "E-Class", "year": 2020}'
car = json.loads(car_string)
print(car["model"])
```

Вивід:

```
{"name": "Анна", "age": 25, "city": "Київ"}
Анна
E-Class
```

## Глибока занурення 

Формат JSON є зручним для обміну даними між різними мовами програмування. Це елегантний, легкий для зрозуміння формат, який підтримує список даних або об'єктів значення-ключ. Крім того, формат JSON є платформонезалежним та підтримується більшістю мов програмування.

## Дивись також

- [Офіційна документація Python для модулю JSON](https://docs.python.org/3/library/json.html)
- [Робота з JSON в Python: керівництво для початківців](https://realpython.com/python-json/)
- [Візуальне пояснення формату JSON](https://www.json.org/json-uk.html)