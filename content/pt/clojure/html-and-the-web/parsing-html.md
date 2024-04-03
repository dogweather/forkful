---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:41.138354-07:00
description: "Analisar HTML em Clojure envolve extrair programaticamente informa\xE7\
  \xF5es de documentos HTML. Programadores fazem isso para acessar, manipular ou monitorar\u2026"
lastmod: '2024-03-13T22:44:46.194643-06:00'
model: gpt-4-0125-preview
summary: "Analisar HTML em Clojure envolve extrair programaticamente informa\xE7\xF5\
  es de documentos HTML."
title: Analisando HTML
weight: 43
---

## Como fazer:
Clojure não possui capacidades de análise de HTML internamente, mas você pode utilizar bibliotecas Java ou invólucros Clojure como `enlive` ou `hickory`. Aqui está como usar ambos:

### Usando Enlive:
Enlive é uma escolha popular para análise de HTML e raspagem web. Primeiro, inclua-o nas dependências do seu projeto:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Então, você pode analisar e navegar por HTML assim:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

Este trecho de código busca uma página HTML e seleciona todos os elementos `<div>` com a classe `some-class`.

A saída pode parecer com:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Aqui está algum conteúdo."]})
```

### Usando Hickory:
Hickory oferece uma maneira de analisar HTML em um formato que é mais fácil de trabalhar em Clojure. Adicione Hickory às dependências do seu projeto:

```clojure
[hickory "0.7.1"]
```

Aqui está um exemplo simples:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Analise o HTML no formato Hickory
(let [doc (hickory/parse "<html><body><div id='main'>Olá, mundo!</div></body></html>")]
  ;; Selecione o div com id 'main'
  (select/select (select/id "main") doc))
```

Este código analisa uma string HTML simples e usa um seletor CSS para encontrar um `div` com o ID `main`.

Exemplo de saída:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Olá, mundo!"]}]
```

Tanto `enlive` quanto `hickory` oferecem soluções robustas para análise de HTML em Clojure, com `enlive` focando mais em templating e `hickory` enfatizando transformação de dados.
