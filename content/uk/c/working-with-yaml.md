---
title:                "Робота з YAML"
date:                  2024-01-19
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
YAML — це формат даних для конфігурації, що легко читається людиною. Програмісти використовують його для налаштування програм, середовищ і для обміну даними між службами та додатками.

## How to: (Як це робити:)
```C
#include <stdio.h>
#include <yaml.h>

int main() {
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_token_t  token;

    if(!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);
    if(fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type) {
        /* Token types are PROCESSED HERE */
        case YAML_STREAM_START_TOKEN: puts("Start Stream"); break;
        case YAML_STREAM_END_TOKEN:   puts("End Stream");   break;
        // Handle other tokens...
        default: /* Do nothing */; 
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

*Sample output:*
```
Start Stream
End Stream
```

## Deep Dive (Занурення у контекст):
YAML створено у 2001 році як зручну альтернативу XML. Зараз існує кілька бібліотек для роботи з YAML у C, такі як libyaml (демонструється вище). Ця бібліотека дає детальний інтерфейс для аналізу та генерації YAML даних. YAML робить легшим серіалізацію структур даних, але вимагає пильності через свої особливості обробки типів і відступів.

## See Also (Додатково):
- Official YAML website: [http://yaml.org](http://yaml.org)
- libyaml GitHub repository: [https://github.com/yaml/libyaml](https://github.com/yaml/libyaml)
- YAML 1.2 specification: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- An article about YAML syntax: [https://en.wikipedia.org/wiki/YAML](https://en.wikipedia.org/wiki/YAML)
- Stack Overflow discussions on YAML usage in C: [https://stackoverflow.com/questions/tagged/yaml?tab=Newest](https://stackoverflow.com/questions/tagged/yaml?tab=Newest)
