---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:20.916772-07:00
description: "JSON (JavaScript Object Notation) - \u0446\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0456\u043D\
  \u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\u0439 \u0448\u0438\
  \u0440\u043E\u043A\u043E \u0437\u0430\u0441\u0442\u043E\u0441\u043E\u0432\u0443\u0454\
  \u0442\u044C\u0441\u044F \u0432 \u0432\u0435\u0431-\u0434\u043E\u0434\u0430\u0442\
  \u043A\u0430\u0445 \u0434\u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\
  \u0430\u043D\u0438\u043C\u0438 \u043C\u0456\u0436 \u043A\u043B\u0456\u0454\u043D\
  \u0442\u0430\u043C\u0438 \u0442\u0430\u2026"
lastmod: 2024-02-19 22:05:09.347197
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) - \u0446\u0435 \u043B\u0435\u0433\u043A\
  \u0438\u0439 \u0444\u043E\u0440\u043C\u0430\u0442 \u043E\u0431\u043C\u0456\u043D\
  \u0443 \u0434\u0430\u043D\u0438\u043C\u0438, \u044F\u043A\u0438\u0439 \u0448\u0438\
  \u0440\u043E\u043A\u043E \u0437\u0430\u0441\u0442\u043E\u0441\u043E\u0432\u0443\u0454\
  \u0442\u044C\u0441\u044F \u0432 \u0432\u0435\u0431-\u0434\u043E\u0434\u0430\u0442\
  \u043A\u0430\u0445 \u0434\u043B\u044F \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\
  \u0430\u043D\u0438\u043C\u0438 \u043C\u0456\u0436 \u043A\u043B\u0456\u0454\u043D\
  \u0442\u0430\u043C\u0438 \u0442\u0430\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
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
