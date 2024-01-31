---
title:                "Praca z JSON"
date:                  2024-01-19
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
JSON, czyli JavaScript Object Notation, to format wymiany danych. Programiści używają go, bo jest lekki, czytelny dla ludzi i maszyn, świetnie nadaje się do komunikacji między serwerem a aplikacjami webowymi lub mobilnymi.

## How to: (Jak to zrobić?)
W C korzystamy z bibliotek, jak `cJSON` lub `Jansson`, do pracy z JSON. Poniżej znajdziesz przykład z wykorzystaniem `cJSON`.

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // Tworzenie obiektu JSON
    cJSON *json = cJSON_CreateObject();
    cJSON_AddStringToObject(json, "name", "Jan Kowalski");
    cJSON_AddNumberToObject(json, "age", 30);
    
    // Drukowanie JSONa
    char *my_json_string = cJSON_Print(json);
    printf("%s\n", my_json_string);
    
    // Czyszczenie
    cJSON_Delete(json);
    free(my_json_string);

    return 0;
}
```

Wyjście:

```
{
        "name": "Jan Kowalski",
        "age": 30
}
```

## Deep Dive (Dogłębna analiza)
JSON pojawił się w 2001 roku. Jego twórcą jest Douglas Crockford. Alternatywą dla JSON jest XML, ale JSON często wygrywa dzięki prostocie i mniejszej objętości danych.

Przy pracy z JSON w C musimy zatroszczyć się o rzeczy jak zarządzanie pamięcią, przewidziane przez bibliotekę, z której korzystamy. Warto też zaznaczyć, że format JSON nie wspiera komentarzy, co jest jego ograniczeniem.

## See Also (Zobacz też)
- cJSON GitHub Page: https://github.com/DaveGamble/cJSON
- Jansson documentation: http://www.digip.org/jansson/
- JSON official website: https://www.json.org/json-en.html
- Porównanie JSON i XML: https://www.json.com/json-vs-xml
