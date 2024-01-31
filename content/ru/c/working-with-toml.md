---
title:                "Работа с TOML"
date:                  2024-01-29T00:05:02.342288-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-toml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
TOML — это язык сериализации данных, разработанный для того, чтобы быть легким для чтения и написания. Программисты используют его для файлов конфигурации, простого хранения данных и обмена данными между языками из-за его ясности и дружелюбности к человеку.

## Как это сделать:
Давайте разберем файл конфигурации TOML в C с использованием библиотеки "tomlc99". Сначала установите библиотеку. Затем создайте `config.toml`:

```toml
title = "Пример TOML"

[owner]
name = "Том Престон-Вернер"
dob = 1979-05-27T07:32:00Z
```

Теперь разберем его в C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Ошибка: не удается открыть файл конфигурации\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Ошибка: %s\n", errbuf);
        return 1;
    }

    printf("Название: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Имя владельца: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Пример вывода:
```
Название: "Пример TOML"
Имя владельца: "Том Престон-Вернер"
```

## Подробнее
TOML, что означает "Tom's Obvious, Minimal Language" (Очевидный, минималистичный язык Тома), был создан Томом Престон-Вернером в 2013 году. Он служит более простой альтернативой форматам, таким как XML и YAML, с акцентом на легкость чтения и написания для человека. Хотя JSON является еще одной альтернативой, TOML сохраняет структуру, которая легче визуально разбирается человеком, что является одной из основных причин его принятия в файлах конфигурации.

В C работа с TOML включает выбор библиотеки парсера, поскольку язык не поддерживает его нативно. Библиотеки, такие как "tomlc99", соответствуют стандарту C99 и предоставляют API для декодирования текста TOML. При учете производительности критически важными являются надлежащая обработка ошибок и управление памятью, так как в C нет встроенного сборщика мусора.

## См. также:
1. Спецификация TOML: [https://toml.io/en/](https://toml.io/en/)
2. GitHub репозиторий tomlc99: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Сравнение форматов сериализации данных: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
