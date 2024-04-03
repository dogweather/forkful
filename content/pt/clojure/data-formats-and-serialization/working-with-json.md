---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:08.140139-07:00
description: "Como fazer: Clojure n\xE3o inclui fun\xE7\xF5es embutidas para trabalhar\
  \ com JSON, ent\xE3o voc\xEA normalmente usar\xE1 bibliotecas de terceiros. `cheshire`\
  \ e `jsonista`\u2026"
lastmod: '2024-03-13T22:44:46.217899-06:00'
model: gpt-4-0125-preview
summary: "Clojure n\xE3o inclui fun\xE7\xF5es embutidas para trabalhar com JSON, ent\xE3\
  o voc\xEA normalmente usar\xE1 bibliotecas de terceiros."
title: Trabalhando com JSON
weight: 38
---

## Como fazer:
Clojure não inclui funções embutidas para trabalhar com JSON, então você normalmente usará bibliotecas de terceiros. `cheshire` e `jsonista` são escolhas populares devido à sua facilidade de uso e desempenho.

### Usando o Cheshire
Primeiro, adicione o Cheshire às dependências do seu projeto em `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Para analisar uma string JSON em um mapa Clojure e converter um mapa para uma string JSON:

```clj
(require '[cheshire.core :as json])

;; Analisar string JSON para mapa Clojure
(let [entrada-json "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string entrada-json true)) ; => {"name" "John", "age" 30}

;; Converter mapa Clojure para string JSON
(let [mapa-clj {"name" "John", "age" 30}]
  (json/generate-string mapa-clj)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Usando o Jsonista
Adicione o Jsonista ao seu projeto `project.clj`:
```clj
[jsonista "0.3.2"]
```

Operações similares com Jsonista:

```clj
(require '[jsonista.core :as j])

;; Analisar string JSON para Clojure
(let [entrada-json "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value entrada-json)) ; => {"name" "Emily", "age" 25}

;; Converter mapa Clojure para string JSON
(let [mapa-clj {"name" "Emily", "age" 25}]
  (j/write-value-as-string mapa-clj)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

Em ambas as bibliotecas, você tem a opção de codificar e decodificar estruturas de dados mais complexas, e existem funções e parâmetros adicionais que permitem a personalização dos processos de serialização e deserialização. Para a maioria das aplicações, a funcionalidade demonstrada fornece uma base sólida para trabalhar com JSON em aplicações Clojure.
