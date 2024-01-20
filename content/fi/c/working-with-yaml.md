---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
YAML on dataformaatti, jota käytetään konffitiedostoihin. Ohjelmoijat suosivat sitä sen luettavuuden ja yksinkertaisuuden vuoksi.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>
#include <yaml.h>

int main() {
    FILE *fh = fopen("config.yaml", "r");
    yaml_parser_t parser;
    yaml_token_t token;

    if(!yaml_parser_initialize(&parser)) {
        printf("Failed to initialize YAML parser!\n");
        return 1;
    }

    if(fh == NULL) {
        printf("Failed to open file!\n");
        return 1;
    }

    yaml_parser_set_input_file(&parser, fh);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type) {
        /* Token types handlers */
        case YAML_STREAM_END_TOKEN:
            break;
        default:
            printf("Got token of type %d\n", token.type);
        }
        if (token.type != YAML_STREAM_END_TOKEN) {
            yaml_token_delete(&token);
        }
    } while (token.type != YAML_STREAM_END_TOKEN);
    
    yaml_token_delete(&token);
    yaml_parser_delete(&parser);
    fclose(fh);
    return 0;
}
```
Sample output:
```
Got token of type 5
Got token of type 257
...
```

## Deep Dive (Syväsukellus)
YAML (YAML Ain't Markup Language) on kehitetty alun perin vuonna 2001. Vaihtoehtoisia formaatteja ovat JSON ja XML. C-kielessä käytetään esim. libyaml-kirjastoa YAML-tiedostojen käsittelyyn.

## See Also (Lisätietoa)
- YAML-standardeja: https://yaml.org/spec/
- libyaml, YAML-parserikirjasto: https://github.com/yaml/libyaml
- YAML ja JSON vertailu: https://en.wikipedia.org/wiki/YAML#Comparison_with_JSON