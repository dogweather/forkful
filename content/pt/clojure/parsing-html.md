---
title:                "Análise de HTML."
html_title:           "Clojure: Análise de HTML."
simple_title:         "Análise de HTML."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Por Que

Você provavelmente está se perguntando por que deveria se importar em analisar HTML com Clojure. Bem, a resposta é simples: o uso de Clojure pode simplificar bastante o processo de análise de HTML e torná-lo mais eficiente. Além disso, como Clojure é uma linguagem funcional e orientada a dados, é mais fácil manipular e extrair informações do HTML sem precisar escrever muito código.

## Como Fazer

Agora que você entende por que deveria considerar usar Clojure para analisar HTML, vamos dar uma olhada em como fazer isso. Para começar, vamos usar a biblioteca clj-html para fazer o parsing do HTML. Você pode instalá-la facilmente pelo Leiningen, adicionando `[clj-html "0.2.2"]` ao seu arquivo `project.clj`.

Uma vez que a biblioteca esteja instalada, podemos começar a analisar nosso HTML. Vamos supor que queremos extrair todos os links de uma página da web. Aqui está um exemplo simples de como fazer isso:

```
(ns meu-app.core
  (:require [clj-html.core :as html]))

(def codigo-html "<html><body><a href='www.exemplo.com'>Link</a><a href='www.outroexemplo.com'>Outro Link</a></body></html>")

(def codigo-parse (html/parse codigo-html))

(map :attrs (html/select codigo-parse [:a]))
;; => ({:href "www.exemplo.com"} {:href "www.outroexemplo.com"})
```

Como você pode ver, a função `html/parse` nos permite transformar nosso código HTML em uma estrutura de dados que podemos manipular facilmente. Em seguida, usamos a função `:attrs` para extrair os atributos dos elementos `a` e `html/select` para selecionar todos os elementos `a` no documento.

## Deep Dive

Se você é novo em Clojure, é possível que esteja se perguntando o que está acontecendo no código acima. Deixe-me explicar com um pouco mais de detalhes.

Em Clojure, os dados são sempre representados como listas, vetores, sets ou maps. Isso é muito útil quando estamos trabalhando com HTML, já que podemos utilizar funções de alto nível para navegar e manipular essas estruturas de dados.

A função `html/parse` retorna uma estrutura de dados chamada AST (Árvore Sintática Abstrata), que é basicamente uma representação hierárquica do nosso código HTML. Em seguida, usamos a função `html/select` para selecionar elementos específicos dessa árvore, usando uma sintaxe semelhante a um seletor CSS.

A partir daí, é apenas uma questão de extrair os atributos que estamos interessados ​​usando a função `:attrs`.

## Veja Também

- [Documentação da biblioteca clj-html](https://github.com/r0man/clj-html)
- [Tutorial de Clojure para Iniciantes](https://github.com/abulimov/clojure-for-newbies)
- [Como usar Clojure para ciência de dados](https://blog.avenuecode.com/clojure-for-data-science)