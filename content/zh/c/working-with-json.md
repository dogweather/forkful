---
title:                "处理JSON数据"
date:                  2024-01-19
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"

category:             "C"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
JSON（JavaScript Object Notation）是存储和交换信息的轻量级格式。程序员用它是因为它易于阅读，且被多种编程语言广泛支持。

## How to: (如何操作：)
在C中处理JSON的常用库是`cJSON`。下面展示如何使用：

```c
#include <stdio.h>
#include "cJSON.h"

// 解析JSON字符串
void parse_json(const char *json_string) {
    cJSON *json = cJSON_Parse(json_string);
    if (json == NULL) {
        fprintf(stderr, "Error before: [%s]\n", cJSON_GetErrorPtr());
        return;
    }

    // 获取字段
    cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Name: %s\n", name->valuestring);
    }

    cJSON_Delete(json);
}

// 生成JSON字符串
void create_json() {
    cJSON *json = cJSON_CreateObject();

    cJSON_AddStringToObject(json, "language", "C");
    cJSON_AddNumberToObject(json, "year", 1972);

    char *json_string = cJSON_Print(json);
    printf("%s\n", json_string);

    cJSON_Delete(json);
    free(json_string);
}

int main() {
    // JSON字符串
    const char *json_data = "{\"name\":\"John\", \"age\":30}";

    // 解析JSON
    parse_json(json_data);

    // 生成JSON
    create_json();

    return 0;
}
```

输出：

```plaintext
Name: John
{"language":"C","year":1972}
```

## Deep Dive (深入探索)
JSON自2002年起成为主流，现在是Web APIs间通信的标准。虽然XML是另一种选择，JSON的轻量级和简洁性使其更受青睐。C程序员通常需要引入第三方库例如`cJSON`或`Jansson`来处理JSON，因为标准库中没有内置JSON支持。`cJSON`简单易用，用于解析和生成JSON结构。

## See Also (另请参阅)
- [cJSON GitHub repository](https://github.com/DaveGamble/cJSON)
- [JSON 官方网站](https://www.json.org/json-zh.html)
- [Jansson: C 的 JSON 库](https://digip.org/jansson/)
