---
title:                "C: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML

Se você é um programador C em busca de uma forma eficiente de gerenciar as configurações e dados estruturados em seus projetos, o YAML pode ser a solução ideal para você! Ele é uma linguagem de marcação simples, legível e muito popular entre desenvolvedores, oferecendo diversas vantagens em relação a outros formatos de dados, como o XML e o JSON.

## Como usar YAML em seus projetos C

Usar YAML em seus projetos C é muito simples e pode te poupar tempo e esforço. Primeiro, você precisa incluir a biblioteca "libyaml" em seu código, utilizando o comando "```C #include <yaml.h>```". Em seguida, você pode manipular os dados em formato YAML utilizando funções específicas da biblioteca, como "yaml_parser_parse()" e "yaml_emitter_emit()". Veja um exemplo abaixo:

```C
#include <yaml.h>

int main() {

    // Criando um parser para ler dados YAML
    yaml_parser_t parser;
    yaml_parser_initialize(&parser);

    // Carregando o arquivo YAML
    FILE *file = fopen("config.yml", "r");
    yaml_parser_set_input_file(&parser, file);

    // Lendo e imprimindo as configurações do arquivo YAML
    yaml_event_t event;
    do {
        if(!yaml_parser_parse(&parser, &event)) {
            printf("Erro ao ler arquivo YAML.\n");
            exit(EXIT_FAILURE);
        }
        switch(event.type) {
            case YAML_SCALAR_EVENT:
                printf("%s\n", event.data.scalar.value);
                break;
            // Outros tipos de eventos aqui
        }
        yaml_event_delete(&event);
    } while(event.type != YAML_STREAM_END_EVENT);

    // Finalizando o parser
    yaml_parser_delete(&parser);

    return 0;
}
```

A saída desse código será a impressão de todos os dados do arquivo YAML, um por linha.

## Aprofundando-se em YAML

Além de ser uma linguagem fácil de utilizar, o YAML também possui diversas funcionalidades avançadas, como a capacidade de incluir e referenciar outros arquivos YAML, tornando-o uma ótima opção para projetos de grande escala. Além disso, ele é altamente configurável, permitindo que você personalize suas configurações de acordo com as necessidades de seu projeto.

Caso queira se aprofundar ainda mais em YAML, uma ótima opção é estudar a documentação oficial da biblioteca "libyaml", que oferece uma lista completa de funções e exemplos de uso.

## Veja também

- Documentação oficial "libyaml": https://pyyaml.org/wiki/LibYAML