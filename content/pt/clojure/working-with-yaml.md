---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê?
YAML, que significa "YAML Ain't Markup Language", é um formato de serialização de dados legível por humanos, comumente usado para arquivos de configuração. Programadores o utilizam pela sua simplicidade e facilidade de leitura/escrita em comparação com formatos como XML ou JSON.

## Como Fazer:
Vamos usar a biblioteca `clj-yaml` para trabalhar com YAML em Clojure. Primeiro, instale adicionando `[clj-yaml "0.7.0"]` ao seu arquivo `project.clj`.

```Clojure
(require '[clj-yaml.core :as yaml])

; Ler YAML de uma string
(def yaml-string "
nome: João
idade: 30
linguagens:
  - Clojure
  - Python
")
(def dados (yaml/parse-string yaml-string))
(println dados)
; Saída: {:nome "João", :idade 30, :linguagens ["Clojure" "Python"]}

; Escrever um mapa Clojure para uma string YAML
(def clojure-map {:nome "Maria", :idade 28, :linguagens ["Ruby" "Elixir"]})
(def yaml-output (yaml/generate-string clojure-map))
(println yaml-output)
; Saída:
; nome: Maria
; idade: 28
; linguagens:
; - Ruby
; - Elixir
```

## Mergulho Profundo
YAML foi introduzido em 2001 e é frequentemente comparado com JSON, outro formato amplamente usado. Enquanto JSON é baseado em estruturas de array e objeto do JavaScript, YAML emprega espaços e indentação para representar hierarquia de dados, favorecendo a legibilidade humana. Além de `clj-yaml`, existem outras bibliotecas como `snakeyaml` (para Java) e `PyYAML` (para Python), que permitem a manipulação de YAML em diferentes ecossistemas de programação. Ao implementar o suporte a YAML, deve-se ter atenção com a segurança, pois a carga de objetos arbitrários pode apresentar riscos.

## Veja Também
- [clj-yaml GitHub repository](https://github.com/clj-commons/clj-yaml)
- [YAML Specification](https://yaml.org/spec/1.2/spec.html)
- [Clojure Official Website](https://clojure.org/)
