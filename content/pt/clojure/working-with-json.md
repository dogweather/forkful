---
title:                "Trabalhando com json"
html_title:           "Clojure: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## O que é e porquê?

Trabalhar com JSON significa manipular dados no formato de JavaScript Object Notation. Programadores o fazem para compartilhar e armazenar informações de forma estruturada e legível por máquina.

## Como fazer:

Usando a biblioteca nativa `clojure.data.json`, é possível converter dados entre formatos de string e tipos de dados do Clojure facilmente:

```Clojure
(require '[clojure.data.json :refer [read-str write-str]])

;; Convertendo uma string JSON em um mapa:
(def json "{:nome \"João\", :idade 25}")
(read-str json)
;; => {:nome "João", :idade 25}

;; Convertendo um mapa em uma string JSON:
(def map {:nome "Maria", :idade 30})
(write-str map)
;; => "{\"nome\":\"Maria\",\"idade\":30}"
```

Também é possível gerar e manipular dados JSON com a biblioteca `cheshire`:

```Clojure
(require '[cheshire.core :refer [generate-string parse-string]])

;; Gerando uma string JSON a partir de um mapa:
(generate-string {:linguagem ["Clojure" "Python" "Java"]})
;; => "{\"linguagem\":[\"Clojure\",\"Python\",\"Java\"]}"

;; Convertendo uma string JSON em uma sequência:
(parse-string "[1, 2, 3]")
;; => [1 2 3]
```

## Profundidade:

JSON foi criado por Douglas Crockford em 2001 e se tornou um formato popular para troca de dados devido à sua simplicidade e legibilidade. Existem outras formas de representar e manipular dados estruturados, como o XML, mas o JSON é amplamente utilizado por sua fácil implementação e interpretação em diversas linguagens de programação.

## Veja também:

- [Documentação da biblioteca clojure.data.json](https://clojure.github.io/data.json/)
- [Documentação da biblioteca cheshire](https://github.com/dakrone/cheshire)
- [Especificação do formato JSON](https://www.json.org/json-pt.html)