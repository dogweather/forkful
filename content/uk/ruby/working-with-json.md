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

## Для чого

Програмування з використанням JSON може бути корисним для зберігання та передавання даних у веб-додатках, а також для взаємодії з API інших сервісів.

## Як

Кодування та декодування даних JSON у Ruby є простим завданням за допомогою методів з модуля `JSON`.

```Ruby
require 'json'

# Кодування об'єкта у JSON рядок
user = {name: 'John', age: 25, city: 'Kyiv'}
user_json = JSON.generate(user)

# Виведення результату
puts user_json
# {"name":"John","age":25,"city":"Kyiv"}

# Декодування JSON рядка у хеш
product_json = '{"name":"Phone", "price": 500, "in_stock": true}'
product = JSON.parse(product_json)

# Виведення значень хеша
puts product["name"]
# Phone
puts product["price"]
# 500
puts product["in_stock"]
# true
```

## Глибоке занурення

Більш складні операції з JSON можуть включати роботу з масивами та вкладеними структурами даних, а також обробку помилок та валідацію даних. Для детальної інформації варто ознайомитися з [офіційною документацією Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html).

## Дивись також

- [Основи JSON для початківців](https://www.codecademy.com/learn/learn-json)
- [Робота з JSON в Ruby on Rails](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-ruby-on-rails-ru)
- [Пакети для роботи з JSON в Ruby](https://rubygarage.org/blog/best-json-gems-for-ruby-on-rails)