---
title:                "Working with JSON"
date:                  2024-01-19
simple_title:         "Working with JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON, short for JavaScript Object Notation, is a lightweight format for data interchange. Programmers use it because it's easy for humans to read and write, and machines to parse and generate, making it a go-to for APIs and config files.

## How to:

In C, you'll often use a library like cJSON or Jansson to handle JSON. Here's how you'd parse and generate JSON with cJSON:

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // JSON we're parsing
    char text[] = "{\"name\": \"John\", \"age\": 30}";

    // Parse JSON
    cJSON *json = cJSON_Parse(text);
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Error before: %s\n", error_ptr);
        }
        return 1;
    }

    // Get values
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(json, "age");

    // Check if the items are valid and of the right type
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Name: %s\n", name->valuestring);
    }
    if (cJSON_IsNumber(age)) {
        printf("Age: %d\n", age->valueint);
    }

    // Clean up
    cJSON_Delete(json);
    return 0;
}
```

Sample Output:
```
Name: John
Age: 30
```

## Deep Dive

JSON was born from JavaScript, but its simplicity made it a standard across many languages. Before JSON, XML was the heavy hitter for data interchange but lacked the minimalism that JSON brought. Lua, YAML, and TOML are alternatives, each with their own use cases and syntax styles. Implementing JSON in C from scratch involves understanding tokens, parsers, and serializers. It's non-trivial, hence the preference for robust libraries.

## See Also

- cJSON Library: https://github.com/DaveGamble/cJSON
- Jansson Library: https://digip.org/jansson/
- JSON Spec: https://www.json.org/json-en.html
- Comparison of data serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
