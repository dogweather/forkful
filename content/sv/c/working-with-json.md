---
title:                "Arbeta med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Arbeta med JSON handlar om att hantera data i JavaScript Object Notation-format, populärt för webbapi:er och konfig-filer. Programmerare använder JSON för att enkelt utbyta data mellan server och klient eller mellan olika programvarusystem.

## How to:

Installation av json-c-biblioteket:
```
sudo apt-get install libjson-c-dev
```
Kodexempel för att läsa JSON:
```C
#include <json-c/json.h>
#include <stdio.h>

int main() {
    const char * jsonString = "{\"name\": \"Coder\", \"language\": \"C\"}";
    struct json_object *parsed_json;
    struct json_object *name;
    struct json_object *language;

    parsed_json = json_tokener_parse(jsonString);

    json_object_object_get_ex(parsed_json, "name", &name);
    json_object_object_get_ex(parsed_json, "language", &language);

    printf("Name: %s\n", json_object_get_string(name));
    printf("Language: %s\n", json_object_get_string(language));

    // Rensa upp
    json_object_put(parsed_json);

    return 0;
}

```
Kör och se:
```
Name: Coder
Language: C
```

## Deep Dive

JSON introducerades 2001. Alternativ inkluderar XML och YAML. JSON är enklare och snabbare, delvis för att det inte kräver avslutande tags. Implementerades först i JavaScript men finns nu i nästan alla programmeringsspråk.

## See Also

- JSON-C GitHub repository: https://github.com/json-c/json-c
- Officiell JSON-webbplats: https://www.json.org/json-en.html
- W3Schools JSON Tutorial: https://www.w3schools.com/js/js_json_intro.asp
