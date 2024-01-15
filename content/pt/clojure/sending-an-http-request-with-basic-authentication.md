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

## Por que

Existem várias situações em que é necessário enviar uma solicitação HTTP com autenticação básica. Por exemplo, ao acessar um API que requer credenciais de autenticação para manter a segurança dos dados transmitidos. Ou ao utilizar uma ferramenta de teste de APIs que suporta apenas esse tipo de autenticação. 

## Como fazer

Aqui estão alguns exemplos de código que mostram como enviar uma solicitação HTTP com autenticação básica usando Clojure:

```Clojure
(require '[clj-http.client :as client])

;; exemplo de uma solicitação GET com autenticação básica
(client/get "http://exemplo.com"
  {:basic-auth ["usuário" "senha"]})

;; exemplo de uma solicitação POST com corpo JSON e autenticação básica
(client/post "http://exemplo.com"
  {:basic-auth ["usuário" "senha"]
   :json-body {:nome "João" :idade 25}})
```

Esses exemplos utilizam a biblioteca `clj-http`, que é uma opção popular para enviar solicitações HTTP em Clojure. O parâmetro `:basic-auth` é onde passamos as credenciais de autenticação em formato de vetor, onde o primeiro elemento é o usuário e o segundo é a senha. Ao usar a função `get` ou `post`, podemos adicionar outras opções, como um cabeçalho personalizado ou um corpo de requisição.

## Profundidade

Para uma compreensão mais profunda, vamos dar uma olhada na estrutura básica de uma solicitação HTTP com autenticação básica:

```
GET /recurso HTTP/1.1
Host: exemplo.com
Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=
```

A primeira linha é o método da solicitação, seguido do path do recurso e da versão do protocolo HTTP utilizado. Em seguida, temos o cabeçalho `Host`, que identifica o servidor que deve receber a solicitação. Por último, o cabeçalho `Authorization` contém informações sobre o tipo de autenticação e as credenciais codificadas em base 64.

Ao receber essa solicitação, o servidor decodifica as credenciais e as compara com as credenciais armazenadas em seu sistema. Se forem válidas, a solicitação é processada normalmente e o servidor responde com os dados solicitados.

## Veja também

- [Documentação oficial da biblioteca clj-http](https://github.com/dakrone/clj-http)
- [Artigo sobre autenticação básica em APIs com Node.js](https://www.digitalocean.com/community/tutorials/nodejs-basic-authentication)
- [Página da MDN sobre autenticação básica em HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication)