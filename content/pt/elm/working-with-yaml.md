---
title:                "Elm: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML

Se você é um programador em busca de uma linguagem de marcação simples e flexível, o YAML pode ser a escolha perfeita. Com sua sintaxe simples e facilidade de leitura, o YAML é ideal para tarefas como configuração de aplicativos e estruturação de dados.

## Como começar a usar o YAML em Elm

Para começar a utilizar o YAML em seus projetos Elm, você precisará primeiro importar o pacote `mortonsc / elm-yaml` em seu código. Em seguida, você pode começar a criar seus dados YAML usando a função `encode` e então decodificá-los usando a função `decode`.

Vamos dar uma olhada em um exemplo simples de como criar e decodificar um arquivo YAML em Elm:

```
import Yaml

-- Criando os dados YAML
yamlData = Yaml.encode
    { "nome": "Sarah",
      "idade": 28,
      "hobbies": ["programar", "ler", "viajar"]
    }

-- Decodificando os dados YAML
decodedData = Yaml.decode yamlData
```

O resultado da função `decode` será um conjunto de dados que podem ser facilmente manipulados em sua aplicação.

## Mergulhando mais fundo no YAML

Ao trabalhar com YAML em Elm, é importante entender que ele é baseado em uma estrutura de chave-valor, semelhante ao JSON. No exemplo acima, "nome", "idade" e "hobbies" são as chaves e seus respectivos valores são "Sarah", 28 e ["programar", "ler", "viajar"].

Além disso, o YAML permite a utilização de listas e objetos aninhados, o que o torna ainda mais versátil para estruturar dados.

Outro aspecto importante a ser considerado é que o YAML é sensível à indentação, o que significa que a estrutura dos dados deve ser corretamente indentada para que possa ser decodificada corretamente.

## Veja também

- [Documentação oficial do pacote Elm-YAML](https://package.elm-lang.org/packages/mortonsc/elm-yaml/latest/)
- [Exemplo de uso do YAML em Elm](https://github.com/mortonsc/elm-yaml/blob/master/demo/src/Main.elm)
- [Tutorial de YAML para iniciantes (em inglês)](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)