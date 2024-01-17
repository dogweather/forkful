---
title:                "Analisando HTML"
html_title:           "Clojure: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por quê?

O parsing HTML é o processo de analisar e interpretar o código HTML de uma página da web. Os programadores fazem isso para extrair informações específicas da página, como tags, links e texto, a fim de usá-las em seus aplicativos.

## Como fazer:

```
; Importando o módulo necessário
(ns meu-aplicativo
  (:require [net.cgrand.enlive-html :as html]))

; Lendo e carregando a página em uma variável
(def pagina (html/html-resource "https://exemplo.com"))

; Obtendo todas as tags <a> da página
(html/select pagina [:a])

; Extraindo os links da página
(def links (map #(-> % :attrs :href) (html/select pagina [:a])))

; Obtendo o texto da página
(def texto (html/text pagina))
```

## Mergulho profundo:

O parsing HTML tem sido uma tarefa fundamental na construção de aplicativos web desde o surgimento da linguagem em 1993 por Tim Berners-Lee. Alternativas como XML e JSON são frequentemente usadas para transferir dados, mas o HTML continua sendo o padrão para a criação de páginas da web. Implementações populares de parsing HTML em Clojure incluem o Enlive e o Hiccup.

## Veja também:

- Enlive: https://github.com/cgrand/enlive
- Hiccup: https://github.com/weavejester/hiccup