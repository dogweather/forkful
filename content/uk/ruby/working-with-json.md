---
title:                "Ruby: Робота з json"
simple_title:         "Робота з json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Зачем

Робота з JSON дозволяє зберігати та обмінюватися даними за допомогою простого та зручного формату. Використовуючи Ruby для обробки JSON, ви можете ефективно робити запити до API, обробляти дані веб-серверів та багато іншого.

## Як використати

Щоб почати роботу з JSON, ви можете скористатися бібліотекою `json` в Ruby. Нижче знаходяться кілька прикладів коду та вихідний результат.

```Ruby
require 'json'

# Створення JSON об'єкта
json_data = '{"name": "Maria", "age": 25, "country": "Ukraine"}'

# Розбір JSON даних та вивід на екран
parsed_data = JSON.parse(json_data)
puts parsed_data["name"] # "Maria"
puts parsed_data["age"] # 25
```

```Ruby
# Створення хеша з даними
ruby_hash = { name: "John", age: 30, country: "Italy" }

# Конвертація хеша в JSON об'єкт та збереження в файл
json_object = JSON.dump(ruby_hash)
File.write("data.json", json_object)
```

Вихідний файл `data.json` буде містити наступний JSON об'єкт:

```Json
{"name": "John", "age": 30, "country": "Italy"}
```

## Покроковий детальний аналіз

JSON, що означає JavaScript Object Notation, є текстовим форматом обміну даними. Він широко використовується для обміну даними веб-серверів та реалізації API. JSON має просту структуру і легкий для використання. Він складається з пари ключ-значення, де ключ є рядком, а значення може бути рядком, числом, булевим значенням, масивом, об'єктом або нульовим значенням.

Для створення та обробки JSON даних в Ruby, вам потрібно скористатися бібліотекою `json`. Ця бібліотека містить класи та методи, які дозволяють легко створювати, читати та зберігати дані в форматі JSON. За допомогою класу `JSON`, ви можете створювати JSON об'єкти, виконувати розбір та серіалізацію даних, а також працювати з HTTP запитами до веб-серверів.

## Детальніше про роботу з JSON

- [Офіційна документація по бібліотеці `json` у Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/json/rdoc/JSON.html)
- [Стаття про роботу з JSON в Ruby на сайті Medium](https://medium.com/swlh/json-in-ruby-parsing-serializing-and-working-with-json-data-in-ruby-3bc62f7cba1c)
- [Приклади використання бібліотеки `json` для роботи з API у Ruby](https://www.twilio.com/blog/working