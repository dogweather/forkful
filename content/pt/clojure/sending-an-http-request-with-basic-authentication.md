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

# Enviando uma requisição HTTP com autenticação básica em Clojure

## O que é e Por quê?

Enviar uma requisição HTTP com autenticação básica é um método comum usado para acesso restrito a informações na Web. Programadores frequentemente fazem isto para acessar APIs seguras ou autenticar clientes em servidores.

## Como fazer:

O Clojure tem uma biblioteca chamada `clj-http` que faz requisições HTTP bastante simples. Primeiro, adicione a dependência `clj-http` ao seu projeto.

```Clojure
[clj-http "3.10.1"]
```

Aqui está um exemplo básico de como fazer uma requisição HTTP com autenticação:

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://exemplo.com" 
                           {:basic-auth ["usuario" "senha"]})]
  (println (:status response))
  (println (:body response)))
```

O código acima primeiro requer o modulo `clj-http.client`, então faz uma requisição GET para "http://exemplo.com" com um nome de usuário e senha fornecidos.

## Mergulho Profundo

Embora o método de autenticação básica seja bastante antigo (introduzido em 1996), continua sendo uma maneira eficaz e simples de fornecer autenticação stateless sobre HTTP. Tem suas desvantagens, como a transmissão de senhas em basicamente texto puro (base64 não é criptografia), mas juntamente com SSL/TLS, ainda é bastante seguro.

Existem alternativas mais seguras, como a autenticação digest, mas a simplicidade da autenticação básica geralmente supera suas deficiências, especialmente quando usada com HTTPS.

Por baixo do capô, `clj-http` está usando Java's `HttpURLConnection` ou apache's `HttpClient` para fazer as requisições sob o capô.

## Veja Também

- Documentação clj-http: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- Basics of HTTP Authentication: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)