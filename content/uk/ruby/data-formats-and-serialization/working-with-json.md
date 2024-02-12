---
title:                "Робота з JSON"
aliases: - /uk/ruby/working-with-json.md
date:                  2024-02-03T19:24:20.916772-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

JSON (JavaScript Object Notation) - це легкий формат обміну даними, який широко застосовується в веб-додатках для обміну даними між клієнтами та серверами. Програмісти працюють з JSON у Ruby, щоб розбирати дані, отримані з зовнішніх джерел, або форматувати дані для відповідей API, використовуючи його легко читабельну структуру для простої маніпуляції даними та зберігання.

## Як:

Ruby, зі своєю стандартною бібліотекою, надає безшовні способи розбору та генерування JSON. Основний модуль для цих операцій - `json`, який можна легко інтегрувати в будь-який додаток Ruby.

### Розбір JSON:

Щоб перетворити рядок JSON на хеш Ruby, ви можете використовувати метод `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Вивід: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Генерування JSON:

Навпаки, щоб перетворити хеш Ruby на рядок JSON, використовуйте метод `JSON.generate` або метод `to_json`, доступний на об'єктах Ruby після того, як бібліотека `json` є підключеною.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Вивід: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Бібліотеки сторонніх розробників:

Хоча стандартна бібліотека Ruby покриває базову обробку JSON, багато проектів покладаються на бібліотеки сторонніх розробників для розширеної функціональності та продуктивності. Одним з популярних варіантів є `Oj` (Optimized JSON).

#### Розбір з Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Вивід: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Генерування з Oj:

Oj також пропонує швидкий спосіб генерування JSON з об'єктів Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Вивід: {"name":"Samantha","age":35,"city":"Miami"}
```

Ці приклади ілюструють прямолінійний характер роботи з JSON у Ruby, що робить його доступним для завдань від простої маніпуляції даними до складних комунікацій API.
