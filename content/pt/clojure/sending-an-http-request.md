---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma requisição HTTP é uma forma de pegar informação de servidores na web. Programadores fazem isso para se comunicar com sites e obter dados deles.

## Como Fazer:

Podemos usar a biblioteca `clj-http` para enviar uma requisição HTTP em Clojure. Aqui está um exemplo simples,

```clojure
(ns my-namespace
  (:require [clj-http.client :as client]))

(defn get-http
  "Enviar uma requisição GET para uma URL e retorna a resposta."
  [url]
  (client/get url))
```

Ao usar essa função, você pode esperar uma resposta como esta,

```clojure
{:status 200
 :headers {"Content-Type" "text/html"}
 :body "<html>...</html>"}
```

## Deep Dive

Historicamente, requisições HTTP têm sido a base da web desde a sua criação. Clojure, sendo uma linguagem moderna, suporta essa operação com facilidade.

Uma alternativa para `clj-http` é a lib `http-kit`. Varia em termos de eficiência e velocidade, dependendo do caso específico.

A implementação do envio de uma requisição HTTP em Clojure é simples, como você pode ver. No entanto, existem muitos detalhes que você pode configurar, como cabeçalhos, parâmetros de consulta, corpo da requisição etc.

## Veja Também

Veja o seguinte para aprender mais:

- Documentação do `clj-http`: https://github.com/dakrone/clj-http
- API de requisição HTTP na MDN: https://developer.mozilla.org/en-US/docs/Web/API/Request