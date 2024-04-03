---
date: 2024-01-26 01:09:27.644485-07:00
description: "Como fazer: Fun\xE7\xF5es em Clojure s\xE3o definidas com `defn`, seguido\
  \ por um nome, par\xE2metros e corpo. Aqui est\xE1 um exemplo r\xE1pido."
lastmod: '2024-03-13T22:44:46.202218-06:00'
model: gpt-4-1106-preview
summary: "Fun\xE7\xF5es em Clojure s\xE3o definidas com `defn`, seguido por um nome,\
  \ par\xE2metros e corpo."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Funções em Clojure são definidas com `defn`, seguido por um nome, parâmetros e corpo. Aqui está um exemplo rápido.

```Clojure
(defn saudar [nome]
  (str "Olá, " nome "!"))

(saudar "Alex") ; => "Olá, Alex!"
```

Agora digamos que queremos calcular a área de um retângulo. Em vez de juntar tudo de qualquer maneira, separamos em duas funções:

```Clojure
(defn area [comprimento largura]
  (* comprimento largura))

(defn imprimir-area [comprimento largura]
  (println "A área é:" (area comprimento largura)))

(imprimir-area 3 4) ; => A área é: 12
```

## Mergulho Profundo
Há tempos atrás, os programadores simplesmente acumulavam toda a sua lógica em um bloco único. Era feio. Depois, a programação estruturada surgiu e funções se tornaram uma coisa. Em Clojure, toda função é de primeira classe — você pode jogá-las por aí como qualquer outro valor.

Alternativas? Algumas pessoas podem brincar com multimétodos ou funções de ordem superior, mas essas são apenas especiarias no guisado de funções.

Todos os detalhes em função da função: elas são imutáveis em Clojure, tornando confusões de efeitos colaterais menos prováveis. Elas dependem fortemente de recursão em vez de loops típicos, o que se encaixa bem com os paradigmas funcionais da linguagem.

## Veja Também
- Guia próprio do Clojure: https://clojure.org/guides/learn/functions
- Noções Básicas de Programação Funcional: https://www.braveclojure.com/core-functions-in-depth/
- Palestras do Rich Hickey: https://changelog.com/posts/rich-hickeys-greatest-hits - para visões sobre a filosofia do Clojure.
