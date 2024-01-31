---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:17.546007-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

JSON, сокращение от JavaScript Object Notation, является легковесным форматом обмена данными. Программисты используют его, потому что его легко читать и писать людям, а также легко анализировать и генерировать машинам, что делает его предпочтительным выбором для API и файлов конфигурации.

## Как это сделать:

В C вы часто будете использовать библиотеку, такую как cJSON или Jansson, для работы с JSON. Вот как вы бы анализировали и генерировали JSON с помощью cJSON:

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // JSON, который мы анализируем
    char text[] = "{\"name\": \"John\", \"age\": 30}";

    // Анализ JSON
    cJSON *json = cJSON_Parse(text);
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Ошибка до: %s\n", error_ptr);
        }
        return 1;
    }

    // Получение значений
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(json, "age");

    // Проверка, являются ли элементы допустимыми и правильного типа
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Имя: %s\n", name->valuestring);
    }
    if (cJSON_IsNumber(age)) {
        printf("Возраст: %d\n", age->valueint);
    }

    // Очистка
    cJSON_Delete(json);
    return 0;
}
```

Пример вывода:
```
Имя: John
Возраст: 30
```

## Глубокое Погружение

JSON возник из JavaScript, но его простота сделала его стандартом среди многих языков. До JSON XML был основным инструментом для обмена данными, но ему не хватало минимализма, который принес JSON. Lua, YAML и TOML являются альтернативами, каждый из которых имеет свои собственные области применения и стили синтаксиса. Реализация JSON в C с нуля включает в себя понимание токенов, парсеров и сериализаторов. Это непростая задача, поэтому предпочтение отдается надежным библиотекам.

## Смотрите также

- Библиотека cJSON: https://github.com/DaveGamble/cJSON
- Библиотека Jansson: https://digip.org/jansson/
- Спецификация JSON: https://www.json.org/json-en.html
- Сравнение форматов сериализации данных: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
