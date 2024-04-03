---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:02.554694-07:00
description: "YAML, um acr\xF4nimo recursivo para \"YAML Ain't Markup Language\" (YAML\
  \ N\xE3o \xE9 uma Linguagem de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\
  \xE3o de dados leg\xEDvel por\u2026"
lastmod: '2024-03-13T22:44:46.216825-06:00'
model: gpt-4-0125-preview
summary: "YAML, um acr\xF4nimo recursivo para \"YAML Ain't Markup Language\" (YAML\
  \ N\xE3o \xE9 uma Linguagem de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\
  \xE3o de dados leg\xEDvel por humanos usado para arquivos de configura\xE7\xE3o\
  \ e troca de dados entre linguagens com diferentes estruturas de dados."
title: Trabalhando com YAML
weight: 41
---

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
