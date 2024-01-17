---
title:                "Trabalhando com csv"
html_title:           "Clojure: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## O que & Por quê?
CSV (Comma-Separated Values) é um formato de arquivo que armazena dados tabulares como uma lista de valores separados por vírgulas. É comumente usado para importar e exportar dados entre aplicativos, principalmente em planilhas. Programadores trabalham com CSV para processar grandes quantidades de dados de forma rápida e eficiente.

## Como fazer:
```Clojure
;; Importando a biblioteca para trabalhar com CSV
(require '[clojure.data :as csv])

;; Lendo um arquivo CSV e armazenando os dados em uma lista de mapas
(def dados-csv (csv/read-csv "dados.csv"))

;; Acessando os dados por meio das chaves dos mapas
(:nome (first dados-csv))
(:idade (second dados-csv))

;; Escrevendo um arquivo CSV a partir de uma lista de mapas
(csv/write-csv "dados.csv" [{:nome "João", :idade 28}, {:nome "Maria", :idade 35}])
```

## Profundando:
CSV é um formato de arquivo com bastante história, tendo sido desenvolvido na década de 1970. Existem várias bibliotecas em diferentes linguagens de programação para trabalhar com ele, mas em Clojure, a biblioteca padrão `clojure.data` é bastante eficiente. Além disso, existem outras opções como `clojure-csv` e `clj-csv` que oferecem diferentes recursos e funcionalidades.

## Veja também:
- [Página oficial da linguagem Clojure](https://clojure.org/)
- [Documentação da biblioteca clojure.data](https://clojure.github.io/clojure/clojure.data-api.html)
- [Mais informações sobre o formato CSV](https://en.wikipedia.org/wiki/Comma-separated_values)