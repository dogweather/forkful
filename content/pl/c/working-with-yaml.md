---
title:                "Praca z yaml"
date:                  2024-01-19
html_title:           "Arduino: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
YAML to format danych, wygodny dla ludzi do czytania i pisania. Programiści używają go bo jest prosty i elastyczny, idealny dla konfiguracji i danych.

## Jak to zrobić:
```C
#include <stdio.h>
#include <yaml.h>

int main(void) {
    FILE *file = fopen("example.yaml", "r");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize parser!\n", stderr);

    if (file == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, file);

    do {
        if (!yaml_parser_parse(&parser, &event)) {
            printf("Parser error %d\n", parser.error);
            exit(EXIT_FAILURE);
        }

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Scalar value: %s\n", event.data.scalar.value);
        }

        if (event.type != YAML_STREAM_END_EVENT)
            yaml_event_delete(&event);

    } while (event.type != YAML_STREAM_END_EVENT);

    yaml_event_delete(&event);
    yaml_parser_delete(&parser);
    fclose(file);

    return 0;
}
```
Wyjście przykładowe:
```
Scalar value: example_value
```

## Deep Dive:
YAML powstał w 2001 roku jako alternatywa dla XML. Jest łatwiejszy do odczytu i pisania niż XML czy JSON, co czyni go popularnym dla konfiguracji aplikacji. Implementacja w C zwykle wykorzystuje libyaml, oficjalną bibliotekę do obsługi YAML, która jest szybka i lekka.

## Zobacz również:
- Specyfikacja YAML: https://yaml.org/spec/1.2/spec.html
- Repozytorium libyaml (biblioteka C): https://github.com/yaml/libyaml
- Tutorial YAML w C z użyciem libyaml: https://www.wpsoftware.net/andrew/pages/libyaml.html
