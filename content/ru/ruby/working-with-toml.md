---
title:                "Работа с TOML"
date:                  2024-01-29T00:04:55.407283-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

TOML — это формат файла конфигурации, который легко читается благодаря своей ясной семантике. Программисты используют TOML для управления конфигурациями приложений и сериализацией данных без избыточности XML или особенностей YAML.

## Как это сделать:

Сначала установите gem `toml-rb`. Это популярный выбор для разбора TOML в Ruby.

```Ruby
gem install toml-rb
```

Затем, чтение TOML файла:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Пример вывода может быть таким:

```
Мое Потрясающее Приложение
```

Запись в TOML файл:

```Ruby
require 'toml-rb'

config = {
  'title' => 'Мое Потрясающее Приложение',
  'owner' => {
    'name' => 'Джон До',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Проверьте `config.toml`, и вы увидите свои настройки, аккуратно сохраненные.

## Погружение

TOML, что означает "Tom's Obvious, Minimal Language" (Очевидный, Минимальный Язык Тома), был создан Томом Престон-Вернером, сооснователем GitHub, примерно в 2013 году. Его основная цель — быть простым форматом, легко преобразуемым в структуры данных. Несмотря на то что JSON отлично подходит для API, а YAML гибкий, ниша TOML — это его акцент на дружелюбии к человеку. В отличие от YAML, который может быть капризным при работе с отступами, TOML стремится к более простой структуре в стиле INI, которую многие находят проще и менее подверженной ошибкам.

Альтернативы вроде JSON, YAML или XML имеют свои сильные стороны, но TOML процветает в сценариях, где конфигурация должна легко поддерживаться как людьми, так и программами. Он не только проще, но и обеспечивает строгое и читабельное форматирование.

С технической стороны, для разбора содержимого TOML с Ruby мы используем gems вроде `toml-rb`. Этот gem использует динамичную природу Ruby, преобразуя данные TOML в родные хеши Ruby, массивы и другие основные структуры данных. Это преобразование означает, что разработчики могут работать с данными TOML, используя знакомую семантику и методы Ruby.

## Смотрите также

- Проект и спецификация TOML: https://toml.io/en/
- Gem `toml-rb`: https://github.com/emancu/toml-rb
- Сравнение TOML, YAML и JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
