---
title:                "Clojure: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma requisição HTTP?

Você pode precisar enviar uma requisição HTTP quando estiver trabalhando com APIs, acessando dados em um servidor ou realizando operações de rede. Isso permite que seu programa se comunique com outros sistemas e serviços.

## Como enviar uma requisição HTTP em Clojure

Para enviar uma requisição HTTP em Clojure, primeiro importe a biblioteca `clj-http.client`:

```
(ns meu-projeto.http
  (:require [clj-http.client :as http]))
```

Em seguida, use a função `http/get` para realizar uma requisição GET em uma URL específica:

```
(def response (http/get "https://meu-site.com/dados"))
```

Você também pode adicionar parâmetros à sua requisição, por exemplo:

```
(def params {:username "joao" :password "12345"})
(def response (http/post "https://meu-site.com/login" {:form-params params}))
```

O objeto `response` conterá uma série de informações úteis, como o corpo da resposta, código de status e cabeçalhos.

## Aprofundando-se nas requisições HTTP

O Clojure oferece suporte a uma variedade de métodos HTTP, incluindo GET, POST, PUT, DELETE e muito mais. Você também pode adicionar cabeçalhos à sua requisição e lidar com autenticação. Para obter mais informações e exemplos, consulte a documentação da biblioteca `clj-http` e a documentação oficial do Clojure.

## Veja também

- Documentação oficial Clojure: https://clojuredocs.org/
- Biblioteca clj-http: https://github.com/dakrone/clj-http
- Documentação HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP