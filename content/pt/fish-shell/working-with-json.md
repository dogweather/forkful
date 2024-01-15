---
title:                "Trabalhando com json"
html_title:           "Fish Shell: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Por quê

Se você está trabalhando com dados em formato JSON, é importante saber como lidar com eles de maneira eficiente. A Fish Shell possui diversas ferramentas que facilitam o trabalho com JSON, tornando o processo de manipulação de dados mais rápido e fácil.

## Como fazer

Abaixo estão alguns exemplos de como utilizar a Fish Shell para trabalhar com JSON em seus projetos:

#### Criando um arquivo JSON

```Fish Shell
set -l data '{"nome": "João", "idade": 25}'
echo $data > arquivo.json
```

O comando acima cria um arquivo chamado "arquivo.json" que contém os dados que definimos na variável `data`. Você pode adicionar mais campos ao JSON, contanto que estejam entre chaves e separados por vírgula.

#### Obtendo dados de um arquivo JSON

```Fish Shell
set -l nome (jq -r '.nome' arquivo.json)
echo "O nome é $nome"
```

O comando acima utiliza o utilitário `jq` para extrair o valor do campo "nome" do arquivo JSON e armazená-lo na variável `nome`. Isso nos permite utilizar os dados do arquivo em outras partes do código.

#### Convertendo JSON para CSV

```Fish Shell
jq -r '(.[0] | keys_unsorted) as $keys | $keys, map([.[ $keys[] ]]) | @csv' arquivo.json > resultado.csv
```

Com essa linha de comando, podemos converter um arquivo JSON para um arquivo CSV. É importante ressaltar que dependendo da estrutura do JSON, pode ser necessário ajustar o código acima para obter o resultado desejado.

## Profundidade

A Fish Shell possui um módulo integrado chamado `fish-json` que oferece funções e variáveis para ajudar a trabalhar com JSON. Você pode ver a documentação completa deste módulo utilizando o comando `man fish_json`. Além disso, é possível instalar módulos adicionais como o `robdennis/fish-jsonpath`, que permite utilizar o formato JSONPath para selecionar dados em um arquivo JSON.

## Veja também

- [Documentação do módulo "fish-json"](https://fishshell.com/docs/current/cmds/fish_json.html)
- [GitHub do módulo "fish-jsonpath"](https://github.com/robdennis/fish-jsonpath)
- [Documentação do JSONPath](https://goessner.net/articles/JsonPath/)