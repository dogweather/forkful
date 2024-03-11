---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:31.316715-07:00
description: "YAML - \u044D\u0442\u043E \u0430\u0431\u0431\u0440\u0435\u0432\u0438\
  \u0430\u0442\u0443\u0440\u0430 \u043E\u0442 \"YAML Ain't Markup Language\" (YAML\
  \ - \u044D\u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\
  \u0435\u0442\u043A\u0438). \u042D\u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442\
  \ \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445, \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0434\
  \u043B\u044F \u0432\u043E\u0441\u043F\u0440\u0438\u044F\u0442\u0438\u044F \u0447\
  \u0435\u043B\u043E\u0432\u0435\u043A\u043E\u043C.\u2026"
lastmod: '2024-03-11T00:14:19.693186-06:00'
model: gpt-4-0125-preview
summary: "YAML - \u044D\u0442\u043E \u0430\u0431\u0431\u0440\u0435\u0432\u0438\u0430\
  \u0442\u0443\u0440\u0430 \u043E\u0442 \"YAML Ain't Markup Language\" (YAML - \u044D\
  \u0442\u043E \u043D\u0435 \u044F\u0437\u044B\u043A \u0440\u0430\u0437\u043C\u0435\
  \u0442\u043A\u0438). \u042D\u0442\u043E \u0444\u043E\u0440\u043C\u0430\u0442 \u0441\
  \u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\u0430\u043D\
  \u043D\u044B\u0445, \u0443\u0434\u043E\u0431\u043D\u044B\u0439 \u0434\u043B\u044F\
  \ \u0432\u043E\u0441\u043F\u0440\u0438\u044F\u0442\u0438\u044F \u0447\u0435\u043B\
  \u043E\u0432\u0435\u043A\u043E\u043C.\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
---

{{< edit_this_page >}}

## Что и Почему?
YAML - это аббревиатура от "YAML Ain't Markup Language" (YAML - это не язык разметки). Это формат сериализации данных, удобный для восприятия человеком. Программисты используют его для файлов конфигурации, обмена данными между языками, потому что он более читаемый, чем JSON или XML для сложных структур данных.

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
