---
title:                "Arbeid med YAML"
date:                  2024-01-19
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML er et dataformat brukt for konfigurasjonsfiler og datautveksling. Programmere bruker det fordi det er lett å lese og skrive, og det fungerer godt med mange programmeringsspråk.

## Hvordan:
```C
#include <stdio.h>
#include <yaml.h>

int main(void) {
    yaml_parser_t parser;
    yaml_token_t token;

    FILE *file = fopen("config.yaml", "r");
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, file);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type) {
        case YAML_KEY_TOKEN: printf("Key: "); break;
        case YAML_VALUE_TOKEN: printf("Value: "); break;
        case YAML_SCALAR_TOKEN: printf("%s\n", token.data.scalar.value); break;
        default: /* Ignorer andre tokens */
            ;
        }
        if(token.type != YAML_STREAM_END_TOKEN)
            yaml_token_delete(&token);
    } while(token.type != YAML_STREAM_END_TOKEN);
    yaml_token_delete(&token);

    yaml_parser_delete(&parser);
    fclose(file);

    return 0;
}
```
_**Output:**_
```
Key: version
Value: 1.0
Key: services
Key: web
Key: image
Value: nginx:latest
```

## Dypdykk

YAML, som betyr "YAML Ain't Markup Language" (opprinnelig "Yet Another Markup Language"), ble introdusert i 2001. Alternativer som JSON og XML eksisterer, men YAML er ofte foretrukket for menneskelig lesbarhet. Det brukes typisk med biblioteker som `libyaml` (C/C++) for parsing/generering.

## Se Også

- YAML offisiell side: https://yaml.org
- LibYAML GitHub-repositorium: https://github.com/yaml/libyaml
- YAML Wikipedia-side: https://no.wikipedia.org/wiki/YAML
