---
title:                "Enviando um pedido http"
html_title:           "Clojure: Enviando um pedido http"
simple_title:         "Enviando um pedido http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Enviar uma solicitação HTTP é essencial para se comunicar com servidores em uma aplicação web. Os programadores fazem isso para obter informações, enviar dados, ou realizar ações através da internet.

## Como fazer:
```Clojure
(ns meu-app.core
  (:require [clj-http.client :as client]))

; GET request
(client/get "http://www.exemplo.com")

; POST request com parâmetros
(def parametros {:nome "João" :idade 25})
(client/post "http://www.exemplo.com/salvar" {:params parametros})

; Resposta do servidor:
; {:status 200
;  :headers {"Content-Type" "text/html"}
;  :body "<html><body>Olá, João!</body></html>"}
```

## Profundidade:
O envio de solicitações HTTP evoluiu com o crescimento da internet e do desenvolvimento de aplicações web. Alternativas para enviar solicitações HTTP incluem bibliotecas de terceiros, como `cljs-ajax` e `http-kit`. A implementação detalhada envolve o uso de sockets para estabelecer uma conexão com o servidor e troca de mensagens em formato de texto.

## Veja também:
- [Documentação do Clj-http](https://github.com/dakrone/clj-http)
- [Comparação entre diversas bibliotecas HTTP para Clojure](https://www.clojure-toolbox.com/compare/http-client-vs-js-capable-http-client)