---
title:                "Trabalhando com YAML"
aliases:
- /pt/clojure/working-with-yaml.md
date:                  2024-02-03T19:25:02.554694-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que é & Por quê?

YAML, um acrônimo recursivo para "YAML Ain't Markup Language" (YAML Não é uma Linguagem de Marcação), é um formato de serialização de dados legível por humanos usado para arquivos de configuração e troca de dados entre linguagens com diferentes estruturas de dados. Programadores aproveitam o YAML devido à sua simplicidade e legibilidade, tornando-o uma escolha ideal para configurar aplicações e facilitar a troca de dados em ambientes de programação poliglota.

## Como fazer:

Clojure não inclui suporte embutido para YAML, mas você pode utilizar bibliotecas de terceiros como `clj-yaml` para analisar e gerar dados YAML. Primeiro, adicione a biblioteca às dependências do seu projeto:

```clojure
;; Adicione isso às dependências do seu project.clj
[clj-yaml "0.7.0"]
```

Aqui está como você pode usar `clj-yaml` para analisar YAML e converter mapas Clojure para YAML.

### Analisando YAML:

```clojure
(require '[clj-yaml.core :as yaml])

;; Analisando uma string YAML
(let [yaml-str "nome: João Doe\nidade: 30\nlinguagens:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; Saída:
;; => {"nome" "João Doe", "idade" 30, "linguagens" ["Clojure" "Python"]}
```

### Gerando YAML a partir de Clojure:

```clojure
(require '[clj-yaml.core :as yaml])

;; Convertendo um mapa Clojure para uma string YAML
(let [data-map {:nome "Jane Doe" :idade 28 :linguagens ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; Saída:
; "idade: 28\nlinguagens:\n- Java\n- Ruby\nnome: Jane Doe\n"
```

Estas operações simples com `clj-yaml` podem ser integradas em aplicações Clojure para lidar com arquivos de configuração ou facilitar a troca de dados com outros serviços ou componentes que usam YAML.
