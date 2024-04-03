---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:20.916772-07:00
description: "\u042F\u043A: Ruby, \u0437\u0456 \u0441\u0432\u043E\u0454\u044E \u0441\
  \u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u043E\u044E \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u043E\u044E, \u043D\u0430\u0434\u0430\u0454 \u0431\
  \u0435\u0437\u0448\u043E\u0432\u043D\u0456 \u0441\u043F\u043E\u0441\u043E\u0431\u0438\
  \ \u0440\u043E\u0437\u0431\u043E\u0440\u0443 \u0442\u0430 \u0433\u0435\u043D\u0435\
  \u0440\u0443\u0432\u0430\u043D\u043D\u044F JSON. \u041E\u0441\u043D\u043E\u0432\u043D\
  \u0438\u0439 \u043C\u043E\u0434\u0443\u043B\u044C \u0434\u043B\u044F \u0446\u0438\
  \u0445 \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u0439 - `json`, \u044F\u043A\u0438\
  \u0439 \u043C\u043E\u0436\u043D\u0430\u2026"
lastmod: '2024-03-13T22:44:50.263852-06:00'
model: gpt-4-0125-preview
summary: "Ruby, \u0437\u0456 \u0441\u0432\u043E\u0454\u044E \u0441\u0442\u0430\u043D\
  \u0434\u0430\u0440\u0442\u043D\u043E\u044E \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A\u043E\u044E, \u043D\u0430\u0434\u0430\u0454 \u0431\u0435\u0437\u0448\
  \u043E\u0432\u043D\u0456 \u0441\u043F\u043E\u0441\u043E\u0431\u0438 \u0440\u043E\
  \u0437\u0431\u043E\u0440\u0443 \u0442\u0430 \u0433\u0435\u043D\u0435\u0440\u0443\
  \u0432\u0430\u043D\u043D\u044F JSON."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
