---
title:                "Arbete med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbete med YAML"
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat för att strukturera konfigurationsdata. Med sin läsbarhet och enkelhet föredrar programmerare YAML för att hantera konfigureringsfiler, lagring av data och att kommunicera mellan olika applikationer och tjänster.

## How to:
För att hantera YAML i C krävs ett bibliotek som `libyaml`. Här är ett enkelt exempel:

```C
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_token_t token;
    
    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type) {
        /* Token types handlers */
        case YAML_KEY_TOKEN: printf("(Key) "); break;
        case YAML_VALUE_TOKEN: printf("(Value) "); break;
        /* Handle other tokens */
        }
        if(token.type != YAML_STREAM_END_TOKEN)
            yaml_token_delete(&token);
    } while(token.type != YAML_STREAM_END_TOKEN);
    yaml_token_delete(&token);

    /* Cleanup */
    yaml_parser_delete(&parser);
    fclose(fh);

    return 0;
}
```
Detta program läser en YAML-fil och skriver ut token-typ för varje element.

## Deep Dive
YAML (YAML Ain't Markup Language) skapades under 2000-talets början som ett mer lättläst alternativ till XML. Förutom `libyaml` finns andra implementationer som `yaml-cpp` för C++. YAML används ofta med applikationer som Docker, Kubernetes, och många programmeringsverktyg.

## See Also
- YAML officiell webbplats: [https://yaml.org](https://yaml.org)
- `libyaml` GitHub repo: [https://github.com/yaml/libyaml](https://github.com/yaml/libyaml)
- YAML-tutorials och specifikationer: [https://learnxinyminutes.com/docs/yaml/](https://learnxinyminutes.com/docs/yaml/)
