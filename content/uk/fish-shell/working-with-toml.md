---
title:                "Робота з TOML"
date:                  2024-01-26T04:21:59.189770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з TOML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/fish-shell/working-with-toml.md"
---

{{< edit_this_page >}}

## Що та Чому?
TOML це формат файлу конфігурації, простий для читання та написання людьми, а також легко парситься та генерується машинами. Програмісти використовують TOML для чітких, ієрархічних конфігураційних файлів у проектах, де ключовим є зручність читання.

## Як:
Для читання та маніпуляцій з TOML у Fish, ви можете використати інструмент, наприклад, `yj`, який може конвертувати TOML в JSON. Ось як:

```fish
# Встановлення yj через Fisher
fisher install jorgebucaran/yj

# Конвертування TOML в JSON
echo 'title = "TOML Example"' | yj -tj

# Приклад виводу
{"title":"TOML Example"}
```

Щоб записати TOML, ви виконуєте процес у зворотному порядку:

```fish
# Конвертування JSON в TOML
echo '{"title":"JSON Example"}' | yj -jt

# Приклад виводу
title = "JSON Example"
```

Для серйозних задач розгляньте можливість використання спеціалізованого інструменту CLI для TOML, як-от `toml-cli`.

```fish
# Встановлення toml-cli
pip install toml-cli

# Встановлення значення у файлі TOML
toml set pyproject.toml tool.poetry.version "1.1.4"

# Отримання значення з файлу TOML
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Поглиблений Розгляд
TOML (Tom's Obvious, Minimal Language), представлений Томом Престон-Вернером у 2013 році, подібний до INI, але з визначеною специфікацією та ієрархією даних. JSON і YAML є основними альтернативами, але вони мають свої компроміси: JSON не так зручний для людей, тоді як YAML складніший. Дизайн TOML процвітає у сценаріях, де конфігураційні файли часто підтримуються вручну, забезпечуючи баланс між простотою та виразністю. Що стосується реалізації, то парсери TOML доступні для більшості мов програмування, включно з TomlBombadil для Fish, який може легко вбудовуватись у ваші скрипти.

## Дивись Також
- Офіційна специфікація TOML: https://toml.io
- `yj`, інструмент для конвертації між TOML, JSON, YAML та XML: https://github.com/jorgebucaran/yj
- `toml-cli`, утиліта командного рядка для TOML: https://github.com/sdispater/toml-cli