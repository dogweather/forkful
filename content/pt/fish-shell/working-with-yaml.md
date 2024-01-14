---
title:                "Fish Shell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

A programação com arquivos YAML é essencial para qualquer desenvolvedor que trabalha com automação de tarefas, configurações de servidores e sistemas de gerenciamento de conteúdo. Com a sintaxe simples e legível, o YAML é uma ótima escolha para facilitar o processo de configuração e manutenção de projetos.

## Como fazer:

Confira abaixo alguns exemplos de como trabalhar com YAML no Fish Shell:

```
# Criação de um arquivo YAML
$ echo "name: João
age: 30" > arquivo.yaml

# Acesso a valores específicos
$ cat arquivo.yaml | yq r - name # João

# Adição de um novo valor
$ yq w -i arquivo.yaml cidade "São Paulo"

# Alteração de um valor existente
$ yq w -i arquivo.yaml age 31

# Remoção de um valor
$ yq d -i arquivo.yaml city

# Uso de variáveis para criação de um arquivo YAML
$ set name Maria
$ set age 28
$ echo "name: $name
age: $age" > arquivo.yaml
```

## Mergulho profundo:

Existem várias ferramentas disponíveis para trabalhar com arquivos YAML no Fish Shell, como yq e shyaml. Além disso, o Fish Shell também possui funções internas como "string split" e "string replace" que podem ser úteis para manipular valores em um arquivo YAML.

Outro aspecto importante ao trabalhar com arquivos YAML é a indentação correta. Uma indentação inadequada pode gerar erros ao carregar ou ler os arquivos. É também possível utilizar expressões regulares para fazer buscas e alterações em um arquivo YAML.

## Veja também:

- [Documentação do Fish Shell](https://fishshell.com/docs/current/)
- [Documentação do yq](https://mikefarah.gitbook.io/yq/)