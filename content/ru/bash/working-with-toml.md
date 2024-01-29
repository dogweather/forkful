---
title:                "Работа с TOML"
date:                  2024-01-29T00:04:18.312024-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
TOML, сокращение от Tom's Obvious, Minimal Language (Очевидный, Минимальный Язык Тома), это формат сериализации данных. Программисты выбирают его за простоту и читаемость; он отлично подходит для файлов конфигурации, создает атмосферу, схожую с YAML, но менее громоздкий, чем JSON, для человека.

## Как это сделать:
В первую очередь, установите `toml-cli` для работы с TOML в Bash. Это полезно для чтения или редактирования файлов TOML на лету.

```Bash
# Установите toml-cli, нашего маленького помощника по задачам TOML
pip install toml-cli

# Представьте, что у вас есть файл TOML, 'config.toml'
echo -e 'title = "Демо TOML"\n\n[owner]\nname = "Том"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Чтение значения
toml get config.toml owner.name
# Вывод: Том

# Установка значения
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Совет: Используйте кавычки для ключей с точками или необычными символами!
```

## Подробнее
TOML появился из-за неприязни к неудобствам JSON для людей вокруг 2013 года. Том Престон-Вернер, сооснователь GitHub, хотел чего-то очень читаемого. YAML и INI были альтернативами, но TOML как лучшее из обоих.

Бум, у вас есть вложенные данные и массивы, минус "подводные камни" YAML и фигурные скобки JSON. Теперь TOML это выбор для конфига в Cargo Rust, что говорит о его росте в мире разработки. Он развивается в соответствии со спецификацией, поддерживая строгость и четкость. Вы найдете парсеры почти на любом языке, что делает его широко адаптируемым.

## Смотрите также
- Официальный репозиторий TOML на GitHub: https://github.com/toml-lang/toml
- toml-cli на PyPI: https://pypi.org/project/toml-cli/
- Сравнение форматов сериализации данных: https://ru.wikipedia.org/wiki/Сравнение_форматов_сериализации_данных
