---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

YAML é um formato de serialização legível por humanos usado para configuração de dados. Programadores o utilizam por sua clareza e compatibilidade com várias linguagens de programação.

## Como Fazer:

Para manipular YAML em C, é comum usar a biblioteca `libyaml`. Aqui está um exemplo básico de como ler um arquivo YAML:

```C
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char* filename) {
    FILE *fh = fopen(filename, "r");
    yaml_parser_t parser;
    yaml_token_t  token;   

    if (!yaml_parser_initialize(&parser))
        fputs("Failed to initialize YAML parser!\n", stderr);
    if (fh == NULL)
        fputs("Failed to open file!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    do {
        yaml_parser_scan(&parser, &token);
        switch(token.type)
        {
        case YAML_STREAM_START_TOKEN: puts("YAML Stream Start"); break;
        case YAML_STREAM_END_TOKEN:   puts("YAML Stream End"); break;
        // Process other tokens like YAML_KEY_TOKEN and YAML_VALUE_TOKEN according to your needs
        // ...
        }
        if(token.type != YAML_STREAM_END_TOKEN)
            yaml_token_delete(&token);
    } while(token.type != YAML_STREAM_END_TOKEN);
    yaml_token_delete(&token);

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("example.yaml");

    return EXIT_SUCCESS;
}
```

A saída vai depender do conteúdo de "example.yaml", mas você verá marcadores de início e fim da transmissão YAML.

## Aprofundando

YAML surgiu em 2001 como alternativa ao XML para ser mais legível e simples. Alternativas incluem JSON e TOML. Em C, escolher `libyaml` é prático por ser uma biblioteca C pura, mas existem wrappers para outras bibliotecas como `yaml-cpp` se estiver usando C++.

## Veja Também

- Documentação do `libyaml`: https://pyyaml.org/wiki/LibYAML
- YAML 1.2 especificação: https://yaml.org/spec/1.2/spec.html
- Comparação entre YAML, JSON e TOML: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
