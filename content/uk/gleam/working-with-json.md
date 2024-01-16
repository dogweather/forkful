---
title:                "Робота з json"
html_title:           "Gleam: Робота з json"
simple_title:         "Робота з json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/working-with-json.md"
---

{{< edit_this_page >}}

##Чому

JSON, або JavaScript Object Notation, є одним з найпоширеніших форматів для обміну даними в Інтернеті. Використання JSON дозволяє легко опрацьовувати дані із різних джерел і використовувати їх у своїх програмах. Тому у даній статті ми розглянемо, як працювати з JSON у мові програмування Gleam.

##Як

Найпростішим способом роботи з JSON даними у Gleam є використання модуля `gleam/json`. Щоб почати роботу з JSON, необхідно спочатку імпортувати цей модуль:

```
gleam/json 
```

Для запиту даних у форматі JSON з веб-сайту можна використовувати функцію `gleam/json.from_url` і передати їй посилання на ресурс:

```
let data = json.from_url("https://example.com/data.json")
```

Далі, щоб отримати доступ до значень у цих даних, використовуйте функцію `get` та індекс (або назву) елементу, який вас цікавить. Наприклад, якщо ми хочемо отримати значення поля `name` з першого елемента у нашому JSON, це можна зробити так:

```
let name = get(data, 0, "name")
```

Зверніть увагу, що ми вказуємо індекс елемента у першому аргументі (у цьому випадку це змінна `data`) і назву поля у другому аргументі.

Якщо ви самі створюєте дані у JSON форматі, то для цього також є удобні функції. Наприклад, функція `gleam/json.array` дозволяє створити масив у форматі JSON, передаючи їй список значень:

```
let my_array = json.array(["Gleam", "is", "fun"])
```

Аналогічно, функція `gleam/json.obj` дозволяє створити об'єкт JSON, передаючи їй список пар ключ-значення:

```
let my_obj = json.obj([("name", "John"), ("age", 25)])
```

Також є функції для серіалізації та десеріалізації JSON даних у рядки, що дозволяє зберігати та передавати дані у форматі JSON.

##Vert

Через те, що JSON є текстовим форматом, він легко зрозуміти людиною, але це також означає, що для зберігання більш складних даних, таких як дерева або графи, потрібно використовувати спеціальні структури даних. У мові Gleam для таких випадків є бібліотеки, наприклад `gleam/typed_json`, що дозволяють використовувати типи даних та перевіряти правильність даних.

##Дивіться також

* [