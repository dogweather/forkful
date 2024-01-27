---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Arbeid med JSON (JavaScript Object Notation) handler om å lagre og utveksle enkel datastrukturer. Programmerere bruker det fordi det er tekstbasert, menneskelesbart og lett å parse for maskiner.

## Hvordan:

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main() {
    // Create a JSON object
    struct json_object *jobj = json_object_new_object();

    // Create JSON key-value pairs
    json_object_object_add(jobj, "navn", json_object_new_string("Ola Nordmann"));
    json_object_object_add(jobj, "alder", json_object_new_int(28));
    json_object_object_add(jobj, "aktiv", json_object_new_boolean(1));

    // Print the JSON object
    printf("%s\n", json_object_to_json_string(jobj));

    // Clean up memory
    json_object_put(jobj);

    return 0;
}
```

Utdata:
```
{"navn": "Ola Nordmann", "alder": 28, "aktiv": true}
```

## Deep Dive

JSON ble introdusert i 2001, inspirert av JavaScript, men er språkuavhengig. Alternativer inkluderer XML og YAML, men JSON er ofte foretrukket for dets enkelhet. Implementering i C krever biblioteker som `json-c` eller `jansson`, for parsing og generering av JSON.

## Se Også:

- Offisiell JSON-standardside: https://www.json.org/json-en.html
- `json-c` GitHub-side: https://github.com/json-c/json-c
- JSON-tutorial for C programmerere: https://www.tutorialspoint.com/json_with_c/index.htm
