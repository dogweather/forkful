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

## Що & Чому?
Робота з JSON - це маніпулювання даними у форматі JSON за допомогою програмування на Python. Програмісти це роблять для обміну даними між різними додатками, такими як веб-сайти і мобільні додатки.

## Як це зробити:
```Python
import json

# створення об'єкта JSON
person = {
    "ім'я": "Іван",
    "вік": 25,
    "професія": "програміст"
}

# перетворення об'єкта в формат JSON
person_json = json.dumps(person)

# збереження об'єкта JSON у файл
with open('person.json', 'w') as f:
    f.write(person_json)

# завантаження даних з файлу JSON
with open('person.json', 'r') as f:
    person = json.load(f)

# виведення результату 
print(person)
```
Вивід: ```{'ім'я': 'Іван', 'вік': 25, 'професія': 'програміст'}```

## Детальний аналіз:
JSON (JavaScript Object Notation) був створений у 2001 році як спосіб обміну даними між різними мовами програмування. Інші альтернативи для роботи з даними включають формат XML та CSV. У Python є різні бібліотеки для роботи з JSON, такі як json, simplejson та ujson. Кодування та декодування JSON даних здійснюється за допомогою методів json.dumps() та json.loads() відповідно.

## Див. також:
- [Офіційна документація Python по JSON](https://docs.python.org/3/library/json.html)
- [Відеоурок з роботи з JSON у Python](https://www.youtube.com/watch?v=9N6a-VLBa2I)
- [Корисні приклади з роботи з JSON у Python](https://realpython.com/python-json/)