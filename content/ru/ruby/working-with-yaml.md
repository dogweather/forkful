---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:31.316715-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
