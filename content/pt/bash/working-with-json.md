---
title:                "Trabalhando com json"
html_title:           "Bash: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-json.md"
---

{{< edit_this_page >}}

## O que e por quê?

Trabalhar com JSON é uma maneira de lidar com dados estruturados na linguagem de programação Bash. Programadores o utilizam para armazenar, acessar e manipular informações de uma forma organizada e simplificada.

## Como fazer:

Para começar, é importante ter o utilitário JSON1.1 instalado no seu sistema. Em seguida, você pode usar o comando `jq` para trabalhar com JSON no Bash. Aqui está um exemplo de como converter um arquivo JSON em um arquivo CSV:

```bash
jq -r '. | @csv' arquivo.json > arquivo.csv
```

O resultado será um arquivo CSV com os dados convertidos do arquivo JSON. Você também pode usar filtros e transformações com o comando `jq` para obter as informações desejadas de um arquivo JSON. Por exemplo:

```bash
jq '.fruits | map(.name + " is " + .color)' arquivo.json
```

Este comando irá listar os nomes e cores das frutas especificadas no arquivo JSON.

## Aprofundando:

JSON significa JavaScript Object Notation e foi criado em 2001 por Douglas Crockford. É uma forma de representar dados estruturados que pode ser lida e interpretada facilmente por humanos e máquinas. Alternativas ao JSON incluem XML e YAML, mas JSON se tornou extremamente popular por ser simples de entender e usar.

A implementação do JSON no Bash é feita através do utilitário `jq`, que é escrito em C e mantido pela comunidade. Ele usa a biblioteca jansson para manipular dados JSON. Além disso, existem outras ferramentas e bibliotecas disponíveis para lidar com JSON em outras linguagens de programação, como o JSON Simple para Java e o JSON for Modern C++.

## Veja também:

- [Site oficial do JSON](https://www.json.org/json-en.html)
- [Documentação do utilitário jq](https://stedolan.github.io/jq/)
- [Biblioteca jansson para trabalhar com JSON em C](https://digip.org/jansson/)