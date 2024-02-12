---
title:                "Работа с TOML"
aliases: - /ru/fish-shell/working-with-toml.md
date:                  2024-01-29T00:04:58.088869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
TOML — это формат файла конфигурации, легкий для чтения и написания людьми, а также легкий для разбора и генерации машинами. Программисты работают с TOML для создания четких, иерархических файлов конфигурации в проектах, где ключевым является удобочитаемость.

## Как это делать:
Для чтения и манипуляции с TOML в Fish вы можете использовать инструмент типа `yj`, который может конвертировать TOML в JSON. Вот как:

```fish
# Установить yj через Fisher
fisher install jorgebucaran/yj

# Конвертировать TOML в JSON
echo 'title = "Пример TOML"' | yj -tj

# Пример вывода
{"title":"Пример TOML"}
```

Чтобы записать TOML, вы просто делаете процесс в обратном направлении:

```fish
# Конвертировать JSON в TOML
echo '{"title":"Пример JSON"}' | yj -jt

# Пример вывода
title = "Пример JSON"
```

Для более сложных задач рассмотрите возможность использования специализированного инструмента CLI для TOML, такого как `toml-cli`.

```fish
# Установить toml-cli
pip install toml-cli

# Установить значение в файле TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Получить значение из файла TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Погружение в тему
TOML (Tom's Obvious, Minimal Language), представленный Томом Престон-Вернером в 2013 году, похож на INI, но имеет определенную спецификацию и иерархию данных. Основными альтернативами являются JSON и YAML, но у них есть свои недостатки: JSON не так удобен для людей, в то время как YAML более сложен. Дизайн TOML преуспевает в сценариях, где файлы конфигурации часто поддерживаются вручную, обеспечивая баланс между простотой и выразительностью. Когда дело доходит до реализации, парсеры TOML доступны для большинства языков программирования, включая TomlBombadil для Fish, который может легко интегрироваться в ваши скрипты.

## Смотрите также
- Официальная спецификация TOML: https://toml.io
- `yj`, инструмент для конвертации между TOML, JSON, YAML и XML: https://github.com/jorgebucaran/yj
- `toml-cli`, утилита командной строки для TOML: https://github.com/sdispater/toml-cli
