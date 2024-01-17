---
title:                "Enviando uma solicitação http"
html_title:           "Elm: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Enviar uma solicitação HTTP é um processo em que um programa envia um pedido de informação ou ação para um servidor web. Isso é feito para obter dados, como informações de uma API, enviar formulários ou até mesmo para realizar alterações no servidor. Os programadores utilizam esse recurso para criar aplicações interativas e dinâmicas que possam se comunicar com fontes externas.

## Como fazer:

```Elm
import Http

Http.send request = 
    let
        method = "GET"
        url = "https://exemploapi.com"
        body = Http.emptyBody
        headers = []
    in
        Http.expectStringResponse handleResponse
```

Saída:

`"Olá mundo!"`

No exemplo acima, utilizamos a função `Http.send` para enviar uma solicitação HTTP utilizando o método `GET` para o URL da API de exemplo. Além disso, definimos o corpo como vazio e nenhum cabeçalho adicional é especificado. Então, esperamos uma resposta em texto e lidamos com ela usando a função `handleResponse`.

## Profundidade:

A capacidade de enviar solicitações HTTP é um recurso importante para a maioria das linguagens de programação, pois permite que as aplicações se conectem a uma variedade de recursos externos. No contexto histórico, esse recurso foi amplamente utilizado para a criação de páginas da web dinâmicas em linguagens como o HTML e o JavaScript. No entanto, com o avanço das tecnologias, o uso do envio de solicitações HTTP se tornou popular em linguagens de programação de back-end, como o Elm.

O Elm oferece algumas alternativas para realizar solicitações HTTP, como a biblioteca `elm/http` padrão da linguagem ou outras bibliotecas criadas pela comunidade. Além disso, é possível modificar as opções da solicitação, como o método, o corpo e os cabeçalhos, de acordo com as necessidades do projeto.

## Ver também:

- Documentação oficial do Elm sobre o envio de solicitações HTTP: https://guide.elm-lang.org/effects/http.html
- Explicação detalhada sobre requisições HTTP: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview