---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:18.312024-07:00
description: "TOML, \u0441\u043E\u043A\u0440\u0430\u0449\u0435\u043D\u0438\u0435 \u043E\
  \u0442 Tom's Obvious, Minimal Language (\u041E\u0447\u0435\u0432\u0438\u0434\u043D\
  \u044B\u0439, \u041C\u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u044B\u0439\
  \ \u042F\u0437\u044B\u043A \u0422\u043E\u043C\u0430), \u044D\u0442\u043E \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0432\u044B\u0431\u0438\u0440\u0430\u044E\
  \u0442 \u0435\u0433\u043E \u0437\u0430\u2026"
lastmod: '2024-03-13T22:44:45.412053-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u0441\u043E\u043A\u0440\u0430\u0449\u0435\u043D\u0438\u0435 \u043E\
  \u0442 Tom's Obvious, Minimal Language (\u041E\u0447\u0435\u0432\u0438\u0434\u043D\
  \u044B\u0439, \u041C\u0438\u043D\u0438\u043C\u0430\u043B\u044C\u043D\u044B\u0439\
  \ \u042F\u0437\u044B\u043A \u0422\u043E\u043C\u0430), \u044D\u0442\u043E \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\
  \u0438\u0438 \u0434\u0430\u043D\u043D\u044B\u0445. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0432\u044B\u0431\u0438\u0440\u0430\u044E\
  \u0442 \u0435\u0433\u043E \u0437\u0430\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 TOML"
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
