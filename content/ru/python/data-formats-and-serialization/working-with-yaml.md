---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:59.020340-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u043F\u043E\u0434\u0440\
  \u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\
  \u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\u043E\
  \u043A\u0443\u043C\u0435\u043D\u0442\u043E\u0432 YAML (Yet Another Markup Language\
  \ - \u0415\u0449\u0435 \u041E\u0434\u0438\u043D \u042F\u0437\u044B\u043A \u0420\u0430\
  \u0437\u043C\u0435\u0442\u043A\u0438) \u043D\u0430 Python. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E\u2026"
lastmod: '2024-03-13T22:44:44.309591-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u043F\u043E\u0434\u0440\
  \u0430\u0437\u0443\u043C\u0435\u0432\u0430\u0435\u0442 \u0440\u0430\u0437\u0431\u043E\
  \u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\u043E\
  \u043A\u0443\u043C\u0435\u043D\u0442\u043E\u0432 YAML (Yet Another Markup Language\
  \ - \u0415\u0449\u0435 \u041E\u0434\u0438\u043D \u042F\u0437\u044B\u043A \u0420\u0430\
  \u0437\u043C\u0435\u0442\u043A\u0438) \u043D\u0430 Python. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
---

{{< edit_this_page >}}

## Что и Почему?
Работа с YAML подразумевает разбор и генерацию документов YAML (Yet Another Markup Language - Еще Один Язык Разметки) на Python. Программисты делают это для управления файлами конфигурации, настройками приложений или сериализацией данных, которая легка для чтения и записи человеком.

## Как:
Для работы с YAML в Python вам понадобится `pyyaml`. Установите его, используя:

```Python
pip install pyyaml
```

Читаем файл YAML:

```Python
import yaml

with open('config.yaml', 'r') as stream:
    try:
        config = yaml.safe_load(stream)
        print(config)
    except yaml.YAMLError as exc:
        print(exc)
```

Запись в файл YAML:

```Python
config = {'database': {'host': 'localhost', 'port': 3306}}

with open('config.yaml', 'w') as file:
    yaml.dump(config, file, default_flow_style=False)
```

Вот как выглядит `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
```

## Погружение
YAML был запущен в 2001 году как дружественный для человека стандарт сериализации данных для всех языков программирования. JSON и XML являются альтернативами, но акцент YAML на удобочитаемости выделяет его среди других. При разборе крайне важно использовать `safe_load`, чтобы предотвратить выполнение произвольного кода из-за небезопасного содержимого YAML. `default_flow_style=False` сохраняет вывод в не JSON-подобном стиле, сохраняя удобочитаемость YAML.

## Смотрите также
- Официальная документация PyYAML: https://pyyaml.org/wiki/PyYAMLDocumentation
- Спецификация YAML: https://yaml.org/spec/1.2/spec.html
- Сравнение JSON и YAML: https://csrc.nist.gov/csrc/media/projects/cryptographic-standards-and-guidelines/documents/examples/data-serialization.pdf
