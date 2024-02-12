---
title:                "Работа с TOML"
aliases:
- /ru/c/working-with-toml/
date:                  2024-02-03T18:12:52.300921-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

TOML (Tom's Obvious, Minimal Language — Язык Тома, Очевидный и Минималистичный) — это формат файла конфигурации, который легко читается благодаря своей ясной семантике. Программисты используют его для файлов конфигурации в приложениях, потому что его простота и удобочитаемость делают его отличным выбором по сравнению с форматами вроде XML или JSON в определенных контекстах.

## Как это сделать:

Чтобы работать с TOML в C, сначала вам нужна библиотека, способная анализировать файлы TOML, поскольку стандартная библиотека C не включает эту функциональность. Популярным выбором является `tomlc99`, легковесный парсер TOML для C99. Вот краткое руководство по чтению простого TOML файла конфигурации:

Во-первых, убедитесь, что у вас установлен и правильно подключен в вашем проекте `tomlc99`.

**Пример TOML файла (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**Код на C для анализа этого файла:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Не удается открыть файл");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Ошибка анализа файла\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Сервер базы данных: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Порт %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Вывод:**
```
Сервер базы данных: "192.168.1.1"
Порт 0: 8001
Порт 1: 8001
Порт 2: 8002
```

## Глубокое погружение

TOML был создан Томом Престон-Вернером, сооснователем GitHub, в ответ на ограничения, которые он видел в других форматах файлов конфигурации. Его цель — быть простым и недвусмысленным как для людей, так и для компьютеров, с возможностью чтения и записи без необходимости сложных правил парсинга. В экосистеме C TOML не является таким удобным инструментом, как это может быть в языках более высокого уровня, таких как Rust с его `serde_toml` или Python с `toml`, которые имеют библиотеки с нативной поддержкой. Вместо этого разработчикам на C нужно полагаться на внешние библиотеки вроде `tomlc99`, но это типично с учетом акцента C на минимализм и производительность.

Хотя TOML хвалят за его ясность, при выборе формата файла конфигурации важно учитывать потребности проекта. В сценариях, требующих более сложных структур или взаимодействия с веб-API, JSON или даже YAML могут лучше подойти, несмотря на их повышенную сложность. TOML выделяется в конфигурациях, где важна читаемость и простота, а не обязательно наличие самых продвинутых структур данных.
