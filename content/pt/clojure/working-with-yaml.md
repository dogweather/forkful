---
title:                "Clojure: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML em Clojure?

 YAML é uma linguagem de marcação de fácil leitura e escrita, frequentemente utilizada para configuração de sistemas e transferência de dados. Ao utilizar o Clojure, é possível manipular e gerar arquivos YAML de forma simples e eficiente.

## Como fazer:

```Clojure 
(ns yaml-exemplo.core
  (:require [clojure.java.io :as io])
  (:import [org.yaml.snakeyaml YAML])) ; importando a biblioteca YAML

;; Criando um mapa com dados para serem transformados em YAML
(def dados {:nome "João" :sobrenome "Silva" :idade 28})

;; Utilizando a função dump da biblioteca SnakeYAML para gerar o arquivo YAML
(with-open [out (io/output-stream "dados.yaml")]
  (YAML/dump dados out))

;; Lendo um arquivo YAML e transformando em mapa
(with-open [in (io/reader "dados.yaml")]
  (YAML/load in))

```

## Mergulho Profundo:

Ao trabalhar com YAML em Clojure, é importante ter em mente as diferenças entre o formato YAML e a estrutura de dados do Clojure. Algumas dicas para esta integração entre as duas linguagens:

- Para criar um mapa em YAML, utilize chaves ({}). Por exemplo: `{:nome "Maria" :sobrenome "Santos" :idade 32}`.
- Para criar uma lista em YAML, utilize colchetes ([]). Por exemplo: `["primeiro elemento" "segundo elemento" "terceiro elemento"]`.
- Para criar um conjunto em YAML, utilize colchetes ({}) com o prefixo `#set`. Por exemplo: `#set [1 2 3 4 5]`.
- Ao ler um arquivo YAML, utilize a função `load` e não `load-all`. A função `load` retorna apenas um mapa, enquanto `load-all` retorna uma sequência de mapas.

## Veja também:

- [Site oficial da linguagem YAML](https://yaml.org/)
- [Documentação da biblioteca SnakeYAML](https://bitbucket.org/asomov/snakeyaml/src/default/)
- [Exemplo de uso de YAML em Clojure](https://www.baeldung.com/java-snake-yaml)