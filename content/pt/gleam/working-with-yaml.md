---
title:                "Trabalhando com yaml"
html_title:           "Gleam: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é isso e por que os programadores o usam?

YAML é uma linguagem de marcação leve que é usada para formatar dados humanamente legíveis. Os programadores muitas vezes usam YAML para estruturar e organizar seus dados de forma simples e fácil de entender. É especialmente útil para armazenar configurações e informações de injeção de dependência em aplicativos.

## Como fazer:

```Gleam 
let dados = """
nome: João
idade: 30
profissão: Desenvolvedor
"""
``` 
```Gleam 
let usuario = dados |> YAML.decode
``` 
```Gleam 
IO.println(usuario["nome"])
```
 
Saída: "João"

## Profundidade de mergulho:

O YAML foi criado em 2001 com o objetivo de ser uma sintaxe simples e fácil de usar para representar dados. Ele é frequentemente usado em conjunto com linguagens de programação para fornecer uma maneira mais intuitiva e legível de estruturar dados. Alternativas para YAML incluem JSON e XML, mas o YAML geralmente é preferido por sua sintaxe mais simples e clara. No Gleam, o módulo ```YAML``` fornece funções para decodificar e codificar dados YAML.

## Veja também:

Saiba mais sobre YAML aqui: https://yaml.org/

Leia a documentação do módulo YAML no Gleam: https://gleam.run/modules/yaml/

Confira este artigo sobre como usar YAML em aplicativos web: https://www.geeksforgeeks.org/yaml-yet-another-markup-language/