---
title:                "Trabalhando com yaml"
html_title:           "Fish Shell: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

##Por que usar YAML na linguagem Fish?

YAML (Yet Another Markup Language) é um formato de dados que é comumente usado para fazer configurações em arquivos de texto simples. Ele possui uma sintaxe amigável e é fácil de aprender, o que o torna uma excelente escolha para ser utilizado na linguagem Fish. Além disso, com a chegada da versão atual do Fish, seu suporte para YAML se tornou mais robusto e prático de ser utilizado.

##Como usar YAML na linguagem Fish

Para utilizar o YAML na linguagem Fish, é necessário ter a versão mais recente do Fish instalada em seu sistema. Em seguida, basta seguir os seguintes passos:

1. Crie um arquivo com a extensão ".yaml" e abra-o com o editor de sua preferência. Por exemplo: `meu_arquivo.yaml`

2. Insira os dados desejados seguindo a sintaxe do YAML, que utiliza espaços para delimitar estruturas (não utilize tabs).

3. Para acessar os dados presentes no arquivo, basta usar o comando `yaml` seguido do caminho do arquivo e o nome do dado desejado. Por exemplo: ```Fish Shell
yaml meu_arquivo.yaml meu_dado
```

4. Você também pode utilizar o YAML para armazenar variáveis e depois acessá-las em seu script Fish. Para isso, utilize o comando `set -g VAR (yaml (cat meu_arquivo.yaml) meu_dado)`, onde `VAR` é o nome da variável e `meu_dado` é o nome do dado que será atribuído a ela.

##Aprofundando-se no YAML

O YAML possui um conjunto de regras e convenções que devem ser seguidas para que a sintaxe seja corretamente interpretada. Existem diversas formas de se estruturar um arquivo YAML, como utilizando listas, dicionários e tags.

Além disso, é possível realizar operações e filtragens nos dados utilizando ferramentas como o `jq`, que auxiliam na manipulação de dados em formato JSON (que é semelhante ao YAML). Com isso, é possível criar scripts Fish mais robustos e dinâmicos.

##Veja também

- [Documentação Oficial do Fish](https://fishshell.com/docs/current/)
- [Especificação Oficial do YAML](https://yaml.org/spec/)
- [Projeto Fish no GitHub](https://github.com/fish-shell/fish-shell)
- [Repositório do `jq` no GitHub](https://github.com/stedolan/jq)