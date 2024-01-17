---
title:                "Trabalhando com yaml"
html_title:           "C++: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é e por quê?

O YAML é uma linguagem de marcação de dados, usada pelos programadores para armazenar e transferir informações de forma legível para humanos e máquinas. Ele é comumente usado para configurar e salvar arquivos de configuração, making it a popular choice for managing settings and configurations for software projects. Como a sintaxe do YAML é baseada no formato de lista de objetos, é fácil de aprender e de utilizar.

## Como fazer:

Para utilizar o YAML em um programa C++, você precisará incluir a biblioteca "yaml-cpp" no seu projeto. Em seguida, é possível carregar um arquivo YAML e acessar os dados dentro dele usando a sintaxe do YAML. Por exemplo:

```
#include <yaml-cpp/yaml.h>
#include <iostream>

using namespace YAML;
int main()
{
    YAML::Node config = YAML::LoadFile("config.yaml");
    double value = config["variavel1"].as<double>();

    std::cout << "Valor da variável 1: " << value << "\n";
    return 0;
}
```
A saída do código acima seria "Valor da variável 1: 5.5", se o arquivo "config.yaml" tiver o seguinte conteúdo:

```
variavel1: 5.5
variavel2: "texto"
bool: true
```

## Profundando

O YAML foi desenvolvido em 2001 por Clark Evans e Ingy döt Net como uma alternativa mais legível e fácil de usar a linguagem de marcação XML. Diferente do JSON, o YAML possui recursos como comentários e referências de objetos, que o tornam mais versátil em algumas situações.

Existem algumas alternativas ao YAML, como o JSON, XML e TOML, cada uma com suas próprias vantagens e desvantagens. Caberá ao programador decidir qual é a melhor opção para o seu projeto.

A implementação do YAML no C++ é feita por meio da biblioteca "yaml-cpp", que suporta a maioria das funcionalidades do YAML, incluindo referências e tags.

## Veja também:

- Documentação oficial da biblioteca "yaml-cpp": https://github.com/jbeder/yaml-cpp
- Especificações oficiais do YAML: https://yaml.org/spec/