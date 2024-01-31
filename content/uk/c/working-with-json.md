---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Робота з JSON у C - це парсинг та генерація JSON-даних. Програмісти це роблять, бо формат JSON є універсальним для обміну даними в мережі, легкий для читання людиною та машини.

## How to: (Як робити:)

Використовуємо бібліотеку `cJSON`. Ось як парсимо JSON:

```C
#include <stdio.h>
#include "cjson/cJSON.h"

int main() {
    const char *json_data = "{\"name\": \"Andriy\", \"age\": 30}";
    
    cJSON *json = cJSON_Parse(json_data);
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(json, "age");
    
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Ім'я: %s\n", name->valuestring);
    }

    if (cJSON_IsNumber(age)) {
        printf("Вік: %d\n", age->valueint);
    }

    cJSON_Delete(json);
    return 0;
}
```
Вивід:
```
Ім'я: Andriy
Вік: 30
```

Генерація JSON:

```C
#include <stdio.h>
#include "cjson/cJSON.h"

int main() {
    cJSON *json = cJSON_CreateObject();
    cJSON_AddStringToObject(json, "name", "Olga");
    cJSON_AddNumberToObject(json, "age", 25);
    
    char *rendered_json = cJSON_Print(json);
    printf("%s\n", rendered_json);
    
    cJSON_Delete(json);
    free(rendered_json);
    return 0;
}
```

Вивід:
```
{"name":"Olga","age":25}
```

## Deep Dive (Поглиблений Занурення)

JSON вводиться 2001 року Дугласом Крокфордом. Він швидко стає стандартом для веб-застосунків. Альтернативи: XML, YAML, але JSON перемагає простотою і швидкістю. Бібліотеки для C: `cJSON`, `Jansson`, `json-c`. Важливо звертати увагу на безпеку: перевіряйте вхідні дані, використовуйте останні версії бібліотек.

## See Also (Дивіться також)

- cJSON GitHub: https://github.com/DaveGamble/cJSON
- JSON стандарт: https://www.json.org/json-en.html
- Порівняння JSON бібліотек: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
