---
title:                "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar um pedido HTTP com autenticação básica?

Em muitos casos, os desenvolvedores precisam se comunicar com APIs que exigem autenticação. A autenticação básica é um método simples de autenticação que envia as credenciais do usuário como um nome de usuário e senha em formato codificado. Neste post, vamos explorar como enviar um pedido HTTP com autenticação básica em Clojure.

## Como Fazer

Para enviar um pedido HTTP com autenticação básica em Clojure, você precisará da biblioteca [clj-http](https://github.com/dakrone/clj-http), que facilita a realização de chamadas HTTP. Começamos importando a biblioteca e definindo as credenciais do usuário em uma variável.

```clojure
(ns meu-projeto.core
(:require [clj-http.client :as client]))

(def username "usuário")
(def password "senha")

```

Em seguida, podemos usar a função `basic-auth` da biblioteca `clj-http` para criar uma string contendo as credenciais codificadas em base64.

```clojure
(client/basic-auth username password)
```

Em seguida, podemos passar essa string como segundo argumento da função `client/get` junto com a URL que desejamos enviar o pedido.

```clojure
(client/get "https://exemplo-api.com" {:basic-auth (client/basic-auth username password)})
```

Isso criará um objeto de resposta que pode ser manipulado como desejado. Por exemplo, se queremos imprimir o conteúdo da resposta, podemos usar a função `:body` para acessar o corpo da resposta e a função `println` para imprimi-lo.

```clojure
(println (:body (client/get "https://exemplo-api.com" {:basic-auth (client/basic-auth username password)})))
```

Isso imprimirá o conteúdo da resposta na tela. Lembre-se de que o uso de `println` é apenas para fins de demonstração e, em uma aplicação real, você pode querer manipular os dados de uma maneira diferente.

## Profundidade

Existem alguns detalhes importantes a serem levados em consideração ao enviar um pedido HTTP com autenticação básica. Primeiro, é importante garantir que a biblioteca `clj-http` esteja instalada em seu ambiente de desenvolvimento. Além disso, é importante tomar cuidado com a segurança ao enviar as credenciais do usuário em texto claro, já que elas podem ser facilmente interceptadas se não estiverem sendo transmitidas de maneira segura.

Outro ponto importante é que, caso a API que você esteja se comunicando exija outros tipos de autenticação, como Token Bearer ou OAuth, as etapas podem ser diferentes e o processo para obter as credenciais pode variar bastante.

Em resumo, o envio de um pedido HTTP com autenticação básica em Clojure é uma tarefa bastante simples e direta, mas é importante estar ciente das nuances e precauções necessárias antes de implementá-lo em seu código.

## Veja Também

- [Documentação da biblioteca clj-http](https://github.com/dakrone/clj-http)
- [API da Clojure para chamadas HTTP](https://clojuredocs.org/clojure.core/require)