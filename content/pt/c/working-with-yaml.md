---
title:                "Trabalhando com yaml"
html_title:           "C: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Trabalhar com YAML pode ser benéfico para programadores, pois é uma formatação leve e fácil de ler para arquivos de configuração e estruturas de dados. Além disso, é amplamente suportada por diferentes linguagens de programação, incluindo o C.

## Como trabalhar com YAML em C

Para trabalhar com YAML em C, é necessário primeiro incluir a biblioteca "libyaml" em seu código. Em seguida, é preciso criar um objeto "yaml_parser_t" para analisar o arquivo YAML e um "yaml_emitter_t" para gerar o arquivo YAML.

Dentro do código, utilize as funções da biblioteca "libyaml" para analisar e gerar o arquivo YAML. Por exemplo:

```C
yaml_parser_t parser;
yaml_parser_initialize(&parser);

FILE *arquivo = fopen("dados.yaml", "rb");
yaml_parser_set_input_file(&parser, arquivo);

yaml_event_t evento;

// Obtendo o próximo evento do arquivo YAML
if (!yaml_parser_parse(&parser, &evento)) {
    // Tratar possíveis erros de parsing
}

// Gerando o arquivo YAML
yaml_emitter_emitted_t emitido = yaml_emitter_emit(&emitter, &evento);

// Fechando o arquivo e liberando memória
fclose(arquivo);
yaml_parser_delete(&parser);
```

## Detalhes sobre o trabalho com YAML em C

Ao trabalhar com YAML em C, é importante ter em mente que existem diferentes tipos de dados suportados, como strings, números e booleanos. Além disso, a biblioteca "libyaml" possui funções para manipulação de erros e gerenciamento de memória.

Uma vantagem de usar YAML em C é que é possível trabalhar com estruturas de dados complexas, como listas e mapas, de maneira simples e legível.

## Veja também

- [Documentação da biblioteca libyaml](https://yaml.org/spec/1.2/spec.html)
- [Tutorial de YAML em C](https://www.tutorialspoint.com/yaml/yaml_tutorial.pdf)
- [Exemplos de código em C usando libyaml](https://github.com/yaml/libyaml)