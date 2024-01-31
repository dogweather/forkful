---
title:                "Робота з TOML"
date:                  2024-01-26T04:26:24.197542-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?

TOML - це формат файлу конфігурації, який легко читається завдяки його чіткій семантиці. Програмісти використовують TOML для управління конфігами додатків і серіалізації даних без громіздкості XML або особливостей YAML.

## Як це робити:

Спочатку встановіть gem `toml-rb`. Це популярний вибір для парсингу TOML в Ruby.

```Ruby
gem install toml-rb
```

Далі, читання TOML файлу:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Приклад виводу може бути:

```
My Awesome App
```

Запис у TOML файл:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Перевірте `config.toml` і ви побачите ваші налаштування, акуратно збережені.

## Поглиблений Огляд

TOML, що означає Tom's Obvious, Minimal Language, був створений Томом Престон-Вернером, співзасновником GitHub, близько 2013 року. Його основною метою є бути простим форматом, який легко парситься у структури даних. Хоча JSON чудово підходить для API, а YAML є гнучким, ніша TOML полягає в його акценті на людино-орієнтованості. На відміну від YAML, який може бути прискіпливий до відступів, TOML прагне до більш INI-подібної структури, яку багато хто вважає простішою і менш схильною до помилок.

Альтернативи як JSON, YAML, або XML мають свої сильні сторони, але TOML процвітає в сценаріях, де конфігурація повинна легко підтримуватися людьми та програмами однаково. Він не тільки простіший, але й забезпечує строге та зрозуміле форматування.

З технічного боку, для парсингу вмісту TOML з Ruby, ми використовуємо геми на зразок `toml-rb`. Цей гем використовує динамічну природу Ruby, конвертуючи дані TOML у рідні хеші, масиви Ruby та інші основні структури даних. Це перетворення означає, що розробники можуть працювати з даними TOML, використовуючи знайомі семантику та методи Ruby.

## Дивіться Також

- Проект та специфікація TOML: https://toml.io/uk/
- Гем `toml-rb`: https://github.com/emancu/toml-rb
- Порівняння TOML, YAML та JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
