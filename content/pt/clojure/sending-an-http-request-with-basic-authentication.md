---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?

Envio de uma solicitação HTTP com autenticação básica é um método de adicionar segurança às comunicações entre um cliente e um servidor. Isso é feito exigindo que o cliente inclua um nome de usuário e senha válidos em cada solicitação HTTP enviada ao servidor. Programadores utilizam esse método para proteger dados confidenciais e garantir que as solicitações sejam provenientes de usuários autorizados.

## Como fazer:

```clojure
(ns meu-projeto
  (:require [clj-http.client :as http]))
  
; Exemplo de solicitação GET com autenticação básica
; A variável "resposta" irá conter a resposta do servidor
(def resposta (http/get "https://www.exemplo.com"
                        :basic-auth "<usuário>" "<senha>"))
                        
; Exemplo de solicitação POST com autenticação básica
; A variável "resposta" irá conter a resposta do servidor
(def resposta (http/post "https://www.exemplo.com"
                         {:basic-auth ["<usuário>" "<senha>"]
                          :form-params {"key" "value"}}))
```

## Exploração profunda:

Existem várias maneiras de implementar autenticação em uma solicitação HTTP, sendo a autenticação básica uma das mais antigas e mais simples. No entanto, devido à sua natureza básica, a autenticação básica não é considerada segura. Como alternativa, pode-se utilizar autenticação baseada em tokens ou certificados digitais. A implementação exata pode variar dependendo do servidor e do cliente em uso.

## Veja também:

- Documentação oficial do [clj-http](https://github.com/dakrone/clj-http)
- Explicações detalhadas sobre [autenticação HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)
- Tutorial passo-a-passo de [autenticação básica em Clojure](https://medium.com/@anuradhayamgi/performing-basic-authentication-in-clojure-aef6c8d20a8c)