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

## O que é YAML?

YAML é uma linguagem de marcação de dados que foi criada para ser facilmente legível por humanos e máquinas. É comumente usado para representar estruturas de dados hierárquicas, como configurações para aplicações ou dados para troca entre sistemas.

## Por que os programadores usam YAML?

Os programadores usam YAML porque é uma forma simples e intuitiva de organizar e armazenar dados estruturados. Além disso, YAML é amplamente suportado por muitas linguagens de programação, incluindo Clojure, tornando-o uma escolha popular para troca de dados entre aplicações.

## Como fazer:

``` Clojure 
(require '[clojure-yaml.core :as yaml])

(def data {:nome "João"
           :idade 30
           :hobbies ["leitura" "caminhada" "jardinagem"]})

(yaml/yaml-str data)
```

Output:

``` yaml
nome: João
idade: 30
hobbies:
  - leitura
  - caminhada
  - jardinagem
```

## Mergulho Profundo:

YAML foi criado em 2001 por Clark Evans e representa "YAML Ain't Markup Language". É uma alternativa aos formatos mais verbosos, como XML e JSON. O parsing de YAML é suportado em Clojure através da biblioteca clojure-yaml, mas também pode ser usado com outras linguagens como Python e Ruby.

## Veja Também:

- Site oficial do YAML: https://yaml.org/
- Documentação da biblioteca clojure-yaml: 
https://oliyh.github.io/clojure-yaml/
- Comparação entre vários formatos de dados: https://www.smashingmagazine.com/2018/01/rise-json/