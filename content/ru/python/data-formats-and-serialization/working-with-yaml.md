---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:59.020340-07:00
description: "\u041A\u0430\u043A: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\
  \u044B \u0441 YAML \u0432 Python \u0432\u0430\u043C \u043F\u043E\u043D\u0430\u0434\
  \u043E\u0431\u0438\u0442\u0441\u044F `pyyaml`. \u0423\u0441\u0442\u0430\u043D\u043E\
  \u0432\u0438\u0442\u0435 \u0435\u0433\u043E, \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u044F."
lastmod: '2024-03-13T22:44:44.309591-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 YAML \u0432\
  \ Python \u0432\u0430\u043C \u043F\u043E\u043D\u0430\u0434\u043E\u0431\u0438\u0442\
  \u0441\u044F `pyyaml`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
