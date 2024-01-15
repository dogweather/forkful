---
title:                "Trabalhando com yaml"
html_title:           "Clojure: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Se você está se aventurando no mundo da programação, provavelmente já se deparou com o formato de arquivo YAML. Ele tem sido cada vez mais utilizado em projetos de desenvolvimento de software, especialmente em linguagens como Clojure. Mas por que alguém utilizaria YAML em vez de outras opções? Bem, vamos mergulhar nisso!

## Como utilizar o YAML em Clojure

A primeira coisa que você precisa saber é que YAML é uma linguagem de marcação de dados, o que significa que ela é usada para representar informações em um formato legível tanto para humanos quanto para máquinas. Em Clojure, podemos utilizar a biblioteca `yaml.core` para trabalhar com arquivos YAML. Vamos ver um exemplo simples de como ler e imprimir os dados de um arquivo YAML:

```Clojure
(ns meu-projeto.yesql
  (:require [yaml.core :as yaml]))

(def config (yaml/read-string "meu-config.yaml"))
(println config)
```

Aqui, importamos a biblioteca `yaml.core` e a renomeamos para `yaml` para facilitar a utilização. Em seguida, utilizamos a função `read-string` para ler o conteúdo do arquivo YAML e armazenamos em uma variável chamada `config`. Por fim, imprimimos os dados utilizando a função `println`. Vamos supor que o nosso arquivo YAML tenha o seguinte conteúdo:

```yaml
nome: João
idade: 30
profissão: desenvolvedor
```

A saída do nosso exemplo seria:

```clojure
{:nome "João", :idade 30, :profissão "desenvolvedor"}
```

Viu como é simples? Agora vamos para uma tarefa mais desafiadora: criar um arquivo YAML a partir de um mapa em Clojure. Para isso, podemos utilizar a função `write-string` e passar o mapa como argumento:

```clojure
(yaml/write-string {:linguagem "Clojure", :ano 2007})
```

A saída seria:

```yaml
linguagem: Clojure
ano: 2007
```

## Aprofundando no YAML

Em Clojure, a biblioteca `yaml.core` oferece uma série de funções úteis para trabalhar com arquivos YAML. Por exemplo, podemos utilizar a função `parse-file` para ler e converter o conteúdo de um arquivo YAML diretamente para uma estrutura de dados em Clojure. Além disso, podemos alterar algumas configurações padrão, como a preservação da ordem das chaves do mapa, usando a função `yaml.core/set-parser-settings`.

Uma dica importante ao trabalhar com YAML em Clojure é sempre garantir que os dados estejam corretamente formatados, já que YAML é sensível a espaçamentos e indentação. Então, sempre preste atenção a esses detalhes ao criar ou modificar arquivos YAML.

## Veja também

- [Documentação oficial da biblioteca YAML em Clojure](https://github.com/cheshire-clj/cheshire)
- [Tutorial sobre YAML em Clojure](https://lambdaisland.com/guides/clojure-yaml)