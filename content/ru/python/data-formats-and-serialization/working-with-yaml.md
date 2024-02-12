---
title:                "Работа с YAML"
aliases:
- /ru/python/working-with-yaml.md
date:                  2024-01-29T00:04:59.020340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
