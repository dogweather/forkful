---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Trabalhar com JSON (JavaScript Object Notation) significa manipular dados estruturados, simples e leves. Programadores fazem isso para intercambiar dados de forma eficaz entre diferentes sistemas ou componentes de software, especialmente em APIs e aplicações web.

## Como Fazer:
Para manusear JSON em Clojure, pode-se usar a biblioteca `cheshire`, que é bem direta. Primeiro, inclua a dependência no seu projeto, depois siga os exemplos abaixo.

```clojure
;; Adicione a dependência no seu arquivo project.clj ou deps.edn
;; [cheshire "5.10.1"] ; Verifique a última versão antes de adicionar

;; Importe a biblioteca
(require '[cheshire.core :as json])

;; Parse de JSON para map do Clojure
(json/parse-string "{\"nome\": \"Clojure\", \"legal\": true}")
;; => {"nome" "Clojure", "legal" true}

;; Gerar uma string JSON a partir de um map do Clojure
(json/generate-string {"nome" "Clojure" "legal" true})
;; => "{\"nome\":\"Clojure\",\"legal\":true}"
```

## Mergulho Profundo:
JSON foi proposto por Douglas Crockford em meados dos anos 2000 e rapidamente se tornou o padrão de facto para intercâmbio de dados. Alternativas incluem XML e YAML, mas JSON predomina devido à sua simplicidade. Em Clojure, a conversão entre JSON e estruturas de dados nativas é eficiente graças ao uso da JVM e bibliotecas otimizadas como cheshire, que faz uso da Jackson sob o capô para parse e geração.

## Veja Também:
- Cheshire: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- Documentação oficial do JSON: [https://www.json.org/json-pt.html](https://www.json.org/json-pt.html)
- Comparativo entre JSON e XML: [https://www.w3schools.com/js/js_json_xml.asp](https://www.w3schools.com/js/js_json_xml.asp)