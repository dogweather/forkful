---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:28.475910-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

YAML — это формат сериализации данных, предназначенный для чтения человеком, используемый для файлов конфигурации, обмена данными между языками и хранения данных. Программисты выбирают YAML за его простоту и читаемость, что делает его идеальным для быстрой настройки и разработки.

## Как это сделать:

В C нет встроенного парсинга YAML, поэтому мы используем библиотеку, такую как `libyaml`, для работы с файлами YAML. Вот простой пример разбора файла YAML на C.

Сначала подключите библиотеку:
```C
#include <yaml.h>
```

Затем инициализируйте парсер, откройте файл и начните разбор:
```C
FILE *fh = fopen("config.yaml", "r");
yaml_parser_t parser;
yaml_parser_initialize(&parser);
yaml_parser_set_input_file(&parser, fh);

yaml_event_t event;
/* Чтение последовательности событий */
while (true) {
    if (!yaml_parser_parse(&parser, &event)) {
        printf("Ошибка парсера %d\n", parser.error);
        exit(EXIT_FAILURE);
    }

    if (event.type == YAML_SCALAR_EVENT) {
        printf("Получено скалярное значение: %s\n", event.data.scalar.value);
    }

    if (event.type == YAML_STREAM_END_EVENT) {
        break;
    }

    yaml_event_delete(&event);
}

/* Очистка */
yaml_parser_delete(&parser);
fclose(fh);
```

Пример содержимого `config.yaml`:
```yaml
name: John Doe
age: 30
```

Пример вывода:
```
Получено скалярное значение: name
Получено скалярное значение: John Doe
Получено скалярное значение: age
Получено скалярное значение: 30
```

## Подробный разбор

YAML - это аббревиатура от "YAML Ain't Markup Language". Он появился в начале 2000-х как альтернатива XML для файлов конфигурации с целью обеспечения читаемости для человека. YAML используется во многих инструментах (например, Docker, Kubernetes и др.) и часто предпочитается JSON для конфигураций из-за поддержки комментариев и более чистого синтаксиса.

Распространенные альтернативы C для работы с YAML — это `libyaml` и `yaml-cpp` (хотя последняя предназначена для C++). Эти библиотеки позволяют программам на C/C++ сериализовать и десериализовать данные YAML.

При разборе YAML ваша программа строит дерево в памяти. Узлы в этом дереве могут быть отображениями (подобно словарям или хеш-таблицам), последовательностями (подобно массивам) или скалярами (строки, числа и т. д.). Парсер libyaml управляется событиями, что означает, что он читает поток YAML и генерирует события для каждой встретившейся структуры YAML. Обработка этих событий позволяет вам конструировать или оперировать соответствующими структурами данных.

## См. также

- GitHub `libyaml`: https://github.com/yaml/libyaml
- Официальные спецификации YAML: https://yaml.org/spec/1.2/spec.html
- Учебник "Программирование с libyaml": https://libyaml.docsforge.com/master/programming-with-libyaml/
- Сравнение форматов сериализации данных: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
