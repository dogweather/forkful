---
title:                "Робота з json"
html_title:           "Ruby: Робота з json"
simple_title:         "Робота з json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# Що і чому?

Робота з JSON це процес обробки інформації у форматі JSON в програмі. Розробники зазвичай працюють з JSON, оскільки цей формат є широко використовуваним у веб-розробці та додатках.

# Як?

```Ruby
require 'json'

# Створення об'єкта JSON
json_object = {"name" => "John", "age" => 25}

# Перетворення у JSON рядок
json_string = json_object.to_json

# Зчитування та парсинг JSON файлу
json_data = File.read('file.json')
parsed_data = JSON.parse(json_data)

# Відображення потрібної інформації з об'єкту JSON
puts parsed_data[0]["name"]
```

Вивід:
```
John
```

# Глибокий занурення

JSON був створений Дугласом Крокфордом у 1999 році та швидко став популярним завдяки простоті та відкритості. Існують також альтернативні формати, такі як XML та YAML, але JSON використовує менше місця для зберігання даних та є більш читабельним.

Парсинг та створення об'єкта JSON можна також виконати з використанням методів `load` та `dump` замість `File.read` та `to_json`. Також, якщо потрібно обробляти великі об'єкти JSON, можна використовувати потоковий парсер `JSON::Stream`.

# Дивись також

- [Офіційна документація по Ruby та JSON](https://ruby-doc.org/stdlib-2.3.1/libdoc/json/rdoc/JSON.html)
- [JSON, або що, як та чому?](https://medium.com/nuances-of-programming/json-a1afdf31a5c9)
- [Переглянути приклади коду JSON на GitHub](https://github.com/search?q=language%3Aruby+json)