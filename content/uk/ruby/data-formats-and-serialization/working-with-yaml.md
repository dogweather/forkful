---
title:                "Робота з YAML"
aliases:
- /uk/ruby/working-with-yaml/
date:                  2024-02-03T19:26:50.291014-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
YAML, що розшифровується як YAML Ain't Markup Language (YAML - це не мова розмітки), широко використовується в Ruby для файлів конфігурації та серіалізації даних завдяки його людино-читабельному формату. Програмісти віддають перевагу YAML, коли їм потрібно зберігати або передавати об'єкти даних у читабельному, але водночас структурованому вигляді, спрощуючи завдання, такі як управління конфігурацією, зберігання даних та обмін даними між мовами.

## Як:
Ruby має вбудовану бібліотеку, яка називається Psych, для аналізу та формування YAML. Щоб її використати, спочатку потрібно підключити стандартну бібліотеку YAML. Ось базовий приклад для початку:

```ruby
require 'yaml'

# Хеш, який буде серіалізовано
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Перетворення хешу в YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Зразок Виводу:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Щоб завантажити дані YAML назад в об'єкт Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Зразок Виводу:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Використання сторонніх бібліотек:

Хоча стандартної бібліотеки достатньо для базових завдань, для складніших потреб ви можете розглянути сторонні геми, такі як 'safe_yaml'. Щоб використати такі бібліотеки, спочатку потрібно встановити гем:

```bash
gem install safe_yaml
```

Після цього ви можете використовувати його для безпечного завантаження даних YAML, зменшуючи ризики, такі як створення об'єктів з даних, що контролюються користувачем:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Зразок Виводу:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Цей підхід покращує безпеку ваших операцій з YAML, роблячи його хорошим вибором для програм, які завантажують YAML з ненадійних джерел.
