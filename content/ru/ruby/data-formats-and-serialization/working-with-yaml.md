---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:31.316715-07:00
description: "\u041A\u0430\u043A: \u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\
  \u043E\u0442\u0430\u0442\u044C \u0441 YAML \u0432 Ruby, \u0432\u0430\u043C \u043D\
  \u0443\u0436\u043D\u0430 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\
  \ `yaml`. \u041E\u043D\u0430 \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u0447\
  \u0430\u0441\u0442\u044C\u044E \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\
  \u043E\u0439 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438 Ruby,\
  \ \u0442\u0430\u043A \u0447\u0442\u043E \u043F\u0440\u043E\u0441\u0442\u043E \u043F\
  \u043E\u0434\u043A\u043B\u044E\u0447\u0438\u0442\u0435 \u0435\u0435."
lastmod: '2024-03-13T22:44:46.034582-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u043E\u0431\u044B \u0440\u0430\u0431\u043E\u0442\u0430\u0442\
  \u044C \u0441 YAML \u0432 Ruby, \u0432\u0430\u043C \u043D\u0443\u0436\u043D\u0430\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430 `yaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как:
Чтобы работать с YAML в Ruby, вам нужна библиотека `yaml`. Она является частью стандартной библиотеки Ruby, так что просто подключите ее:

```ruby
require 'yaml'
```

Чтобы преобразовать хэш Ruby в строку YAML:

```ruby
require 'yaml'

my_hash = { name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess'] }

yaml_string = my_hash.to_yaml
puts yaml_string
```

Вывод будет строкой в формате YAML:

```
---
:name: Sam
:occupation: Developer
:hobbies:
- coding
- chess
```

Чтобы загрузить строку YAML в Ruby:

```ruby
require 'yaml'

yaml_string = "
name: Sam
occupation: Developer
hobbies:
  - coding
  - chess
"

ruby_hash = YAML.load(yaml_string)
puts ruby_hash
```

Вывод - это хэш Ruby:

```
{name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess']}
```

## Подробный анализ
YAML появился в начале 2000-х как удобная для человека альтернатива XML для файлов конфигурации и сериализации данных. Его дизайн позволяет легко сопоставлять с нативными структурами данных во многих языках, имея реализации в Python, Ruby, Java, PHP и других.

Альтернативами YAML являются JSON и TOML. JSON более распространен для веб-API из-за его непосредственной совместимости с JavaScript. TOML стремится быть более читаемым как файл конфигурации, предлагая набор функций, аналогичный YAML.

В Ruby YAML реализован с помощью библиотеки Psych, которая является стандартным парсером YAML, начиная с Ruby 1.9.3. Psych взаимодействует с libyaml, библиотекой на C для разбора и генерации YAML.

## Смотрите также
- [Официальный сайт YAML](https://yaml.org/)
- [Документация библиотеки Psych](https://ruby-doc.org/stdlib-3.0.0/libdoc/psych/rdoc/Psych.html)
- [Документация модуля YAML в Ruby](https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html)
- [Официальный сайт JSON (JavaScript Object Notation)](https://www.json.org/)
- [Репозиторий GitHub TOML (Tom's Obvious, Minimal Language)](https://github.com/toml-lang/toml)
