---
title:                "Werken met JSON"
date:                  2024-01-28T22:10:14.624900-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met JSON"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

JSON, de afkorting van JavaScript Object Notatie, is een lichtgewicht formaat voor gegevensuitwisseling. Programmeurs gebruiken het omdat het gemakkelijk te lezen en te schrijven is voor mensen, en eenvoudig te ontcijferen en te genereren voor machines. Dit maakt het een eerste keuze voor API's en configuratiebestanden.

## Hoe te:

In C zal je vaak een bibliotheek zoals cJSON of Jansson gebruiken om met JSON om te gaan. Hier is hoe je JSON zou ontleden en genereren met cJSON:

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // JSON die we ontleden
    char text[] = "{\"name\": \"John\", \"age\": 30}";

    // Parse JSON
    cJSON *json = cJSON_Parse(text);
    if (json == NULL) {
        const char *error_ptr = cJSON_GetErrorPtr();
        if (error_ptr != NULL) {
            fprintf(stderr, "Fout voor: %s\n", error_ptr);
        }
        return 1;
    }

    // Waarden ophalen
    const cJSON *name = cJSON_GetObjectItemCaseSensitive(json, "name");
    const cJSON *age = cJSON_GetObjectItemCaseSensitive(json, "age");

    // Controleren of de items geldig zijn en van het juiste type
    if (cJSON_IsString(name) && (name->valuestring != NULL)) {
        printf("Naam: %s\n", name->valuestring);
    }
    if (cJSON_IsNumber(age)) {
        printf("Leeftijd: %d\n", age->valueint);
    }

    // Opruimen
    cJSON_Delete(json);
    return 0;
}
```

Voorbeelduitvoer:
```
Naam: John
Leeftijd: 30
```

## Dieper Duiken

JSON is ontstaan uit JavaScript, maar door de eenvoud ervan werd het een standaard in vele talen. Voor JSON was XML de grote speler voor gegevensuitwisseling, maar het ontbrak aan de minimalisme die JSON bracht. Lua, YAML en TOML zijn alternatieven, elk met hun eigen gebruiksscenario's en syntaxisstijlen. JSON in C vanaf nul implementeren vereist een begrip van tokens, parsers en serializers. Het is niet triviaal, vandaar de voorkeur voor robuuste bibliotheken.

## Zie Ook

- cJSON Bibliotheek: https://github.com/DaveGamble/cJSON
- Jansson Bibliotheek: https://digip.org/jansson/
- JSON Specificatie: https://www.json.org/json-nl.html
- Vergelijking van gegevensserialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_gegevensserialisatieformaten
