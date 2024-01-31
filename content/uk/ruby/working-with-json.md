---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Що та Чому?
JSON - це формат даних для обміну. Програмісти використовують його через його читабельність людиною і простоту інтеграції з більшістю мов програмування, включно з Ruby.

## Як це робити:
В Ruby для роботи з JSON є вбудований модуль. Нижче - приклади:

```Ruby
require 'json'

# Конвертація рядка JSON в хеш Ruby
json_string = '{"name":"Olivia","age":30,"city":"Kyiv"}'
ruby_hash = JSON.parse(json_string)
puts ruby_hash
# => {"name"=>"Olivia", "age"=>30, "city"=>"Kyiv"}

# Конвертація хешу Ruby в рядок JSON
ruby_hash = { name: 'Andriy', age: 25, city: 'Lviv' }
json_string = ruby_hash.to_json
puts json_string
# => {"name":"Andriy","age":25,"city":"Lviv"}
```

## Поглиблений огляд:
JSON (JavaScript Object Notation) з'явився у 2000 році. Сьогодні є такі альтернативи, як XML та YAML, але JSON залишається популярним через свою легкість використання. В Ruby для роботи з JSON використовують стандартний модуль `json`. Він дозволяє легко парсити JSON-рядки та серіалізувати об'єкти Ruby.

## Дивіться також:
- [Документація модуля JSON для Ruby](https://ruby-doc.org/stdlib-3.1.1/libdoc/json/rdoc/JSON.html)
- [JSON офіційний сайт](https://www.json.org/json-uk.html)
